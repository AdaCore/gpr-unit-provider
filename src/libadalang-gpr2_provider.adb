------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Locks;

with GPR2.Path_Name;
with GPR2.Project.Unit_Info; use GPR2.Project.Unit_Info;
with GPR2.Project.View.Set;
with GPR2.Project.Source;
with GPR2.Project.Source.Set;
with GPR2.Unit;

with Libadalang.Common;
with Libadalang.GPR2_Lock;
with Libadalang.Unit_Files;

package body Libadalang.GPR2_Provider is

   package US renames Ada.Strings.Unbounded;
   use type US.Unbounded_String;

   type GPR2_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Tree     : access GPR2.Project.Tree.Object;
      Projects : GPR2.Project.View.Vector.Object;
   end record;

   overriding function Get_Unit_Filename
     (Provider : GPR2_Unit_Provider;
      Name     : Langkit_Support.Text.Text_Type;
      Kind     : Libadalang.Common.Analysis_Unit_Kind) return String;

   overriding function Get_Unit
     (Provider    : GPR2_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Langkit_Support.Text.Text_Type;
      Kind        : Libadalang.Common.Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;

   overriding procedure Release (Provider : in out GPR2_Unit_Provider);

   function From_Aggregated_Subtree
     (Prj : GPR2.Project.View.Object)
      return GPR2.Project.View.Vector.Object;
   --  Returns list of views for the aggregated subtree

   ------------------------------------------
   -- Helpers to create project partitions --
   ------------------------------------------

   type Files_For_Unit is record
      Spec_File, Body_File : aliased US.Unbounded_String;
   end record;
   --  Identify the source files that implement one unit (spec & body for a
   --  specific unit name, when present).

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : GPR2.Path_Name.Object;
      Part : GPR2.Unit.Library_Unit_Type);
   --  Register the couple File/Part in FFU

   package Unit_Files_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => US.Unbounded_String,
      Element_Type    => Files_For_Unit,
      Equivalent_Keys => US."=",
      Hash            => US.Hash);
   --  Associate a set of files to unit names

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      File       : GPR2.Project.Source.Object);
   --  Wrapper around Set_Unit_File to register the couple File/Part in the
   --  appropriate Unit_Files' entry. Create such an entry if needed.

   type Aggregate_Part is record
      Projects   : GPR2.Project.View.Vector.Object;
      Unit_Files : Unit_Files_Maps.Map;
   end record;
   --  Group of projects that make up one part in the aggregated projects
   --  partition.

   function Part_Image (Part : Aggregate_Part) return String;
   --  Return a human-readable string that represent the set of projects in
   --  Part.

   type Aggregate_Part_Access is access all Aggregate_Part;
   procedure Free is new Ada.Unchecked_Deallocation
     (Aggregate_Part, Aggregate_Part_Access);

   function Try_Merge
     (Part       : in out Aggregate_Part;
      Project    : GPR2.Project.View.Object;
      Unit_Files : in out Unit_Files_Maps.Map) return Boolean;
   --  If all common unit names in Part.Unit_Files and Unit_Files are
   --  associated with the same source files, update Part so that Project is
   --  part of it, clear Unit_Files and return True. Do nothing and return
   --  False otherwise.

   package Aggregate_Part_Vectors is new Ada.Containers.Vectors
     (Positive, Aggregate_Part_Access);
   procedure Free (Partition : in out Aggregate_Part_Vectors.Vector);

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return LAL.Unit_Provider_Reference
   is
      use Ada.Containers;
      use GPR2;
      use GPR2.Project.View;

      Actual_Project : GPR2.Project.View.Object := View;
   begin
      --  If no project was given, try to run the partitionner

      if not Actual_Project.Is_Defined then
         declare
            Result   : LAL.Unit_Provider_Reference;
            PAPs     : Provider_And_Projects_Array_Access :=
               Create_Project_Unit_Providers (Tree);
         begin
            if PAPs.all'Length > 1 then
               Free (PAPs);
               raise Unsupported_View_Error with "inconsistent units found";
            end if;

            --  We only have one provider. Grant ownership to Result if
            --  requested and we are done.

            Result := PAPs.all (PAPs.all'First).Provider;
            Free (PAPs);
            return Result;
         end;
      end if;

      --  Peel the aggregate project layers (if any) around Actual_Project. If
      --  we find an aggregate project with more than one aggregated project,
      --  this is an unsupported case.

      while Actual_Project.Kind = K_Aggregate loop
         declare
            Subprojects : constant GPR2.Project.View.Set.Object :=
               Actual_Project.Aggregated;
            Leave_Loop  : constant Boolean :=
               Subprojects.Length /= 1;
         begin
            if not Leave_Loop then
               Actual_Project := Subprojects.First_Element;
            end if;
            exit when Leave_Loop;
         end;
      end loop;

      if Actual_Project.Kind = K_Aggregate then
         raise Unsupported_View_Error with
            "selected project is aggregate and has more than one sub-project";
      end if;

      declare
         Provider : constant GPR2_Unit_Provider :=
           (Tree     => Tree.Reference,
            Projects =>
              GPR2.Project.View.Vector.Vector.To_Vector (Actual_Project, 1));
      begin
         return LAL.Create_Unit_Provider_Reference (Provider);
      end;
   end Create_Project_Unit_Provider;

   -----------------------------------
   -- Create_Project_Unit_Providers --
   -----------------------------------

   function Create_Project_Unit_Providers
     (Tree : GPR2.Project.Tree.Object)
      return Provider_And_Projects_Array_Access
   is
      use GPR2;
      use GPR2.Project.Source.Set;

      function Unwind_Aggregated
        (Prj : GPR2.Project.View.Object)
         return GPR2.Project.View.Vector.Object;
      --  Returns all non aggregate roots

      -----------------------
      -- Unwind_Aggregated --
      -----------------------

      function Unwind_Aggregated
        (Prj : GPR2.Project.View.Object)
         return GPR2.Project.View.Vector.Object
      is
         Result : GPR2.Project.View.Vector.Object;
      begin
         if Prj.Kind = K_Aggregate then
            for V of Prj.Aggregated loop
               Result.Append_Vector (Unwind_Aggregated (V));
            end loop;
         else
            Result.Append (Prj);
         end if;

         return Result;
      end Unwind_Aggregated;

      Partition : Aggregate_Part_Vectors.Vector;
   begin
      if Trace.Is_Active then
         Trace.Increase_Indent
           ("Trying to partition " & String (Tree.Root_Project.Name));
      end if;

      if Tree.Root_Project.Kind = K_Aggregate then

         --  We have an aggregate project: partition aggregated projects so
         --  that each unit providers (associated to one exclusive set of
         --  projects) has visibility on only one version of a unit.

         declare
            Projects : GPR2.Project.View.Vector.Object :=
              Unwind_Aggregated (Tree.Root_Project);

            function "<" (Left, Right : Project.View.Object) return Boolean is
              (Left.Name < Right.Name);

            package Sorting is new Project.View.Vector.Vector.Generic_Sorting;
         begin
            --  Sort projects by name so that our output is deterministic:
            --  Unwind_Aggregated does not specify the order
            --  of projects in its result.

            Sorting.Sort (Projects);

            --  For each aggregated project...

            Aggregate_Iteration : for P of Projects loop
               declare
                  Unit_Files      : Unit_Files_Maps.Map;
                  New_Part_Needed : Boolean := True;
               begin

                  --  List all units defined and keep track of which source
                  --  files implement them.

                  for Sub_P of From_Aggregated_Subtree (P) loop
                     for Src of Sub_P.Sources loop
                        if Src.Is_Ada then
                           Set_Unit_File (Unit_Files, Src);
                        end if;
                     end loop;
                  end loop;

                  Part_Lookup : for Part of Partition loop
                     if Try_Merge (Part.all, P, Unit_Files) then
                        New_Part_Needed := False;
                        exit Part_Lookup;
                     end if;
                  end loop Part_Lookup;

                  if New_Part_Needed then
                     declare
                        Part : constant Aggregate_Part_Access :=
                           new Aggregate_Part;
                        Success : constant Boolean :=
                           Try_Merge (Part.all, P, Unit_Files);
                     begin
                        pragma Assert (Success);
                        Partition.Append (Part);
                     end;
                  end if;
               end;

            end loop Aggregate_Iteration;
         end;

         --  If the partition is empty (there was no aggregated project),
         --  create one unit provider anyway: this provider will refer to an
         --  empty list of projects.

         if Partition.Is_Empty then
            Partition.Append (new Aggregate_Part);
         end if;

      else
         --  Project is not an aggregate project, so the partition is obvious:
         --  one part that contains the root project.

         declare
            Part : constant Aggregate_Part_Access := new Aggregate_Part;
         begin
            Part.Projects.Append (Tree.Root_Project);
            Partition.Append (Part);
         end;
      end if;

      if Trace.Is_Active then
         Trace.Decrease_Indent;
      end if;

      --  For debuggability, log how the Tree was partitionned

      if Trace.Is_Active then
         Trace.Increase_Indent ("Input project partitionned into:");
         for Cur in Partition.Iterate loop
            declare
               N    : constant Positive :=
                  Aggregate_Part_Vectors.To_Index (Cur);
               Part : Aggregate_Part renames Partition.Element (N).all;
            begin
               Trace.Trace ("Part" & N'Image & ": " & Part_Image (Part));
            end;
         end loop;
         Trace.Decrease_Indent;
      end if;

      --  The partition is ready: turn each part into a unit provider and
      --  return the list.

      return Result : constant Provider_And_Projects_Array_Access :=
         new Provider_And_Projects_Array (1 .. Natural (Partition.Length))
      do
         for I in Result.all'Range loop
            declare
               Part : Aggregate_Part_Access renames Partition (I);
               PUP  : constant GPR2_Unit_Provider :=
                  (Tree     => Tree.Reference,
                   Projects => Part.Projects);
            begin
               Result (I).Projects := Part.Projects;
               Result (I).Provider :=
                  LAL.Create_Unit_Provider_Reference (PUP);
            end;
         end loop;
         Free (Partition);
      end return;
   end Create_Project_Unit_Providers;

   ----------
   -- Free --
   ----------

   procedure Free (Partition : in out Aggregate_Part_Vectors.Vector) is
   begin
      for Part of Partition loop
         Free (Part);
      end loop;
      Partition.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (PAP_Array : in out Provider_And_Projects_Array_Access) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Provider_And_Projects_Array, Provider_And_Projects_Array_Access);
   begin
      for PAP of PAP_Array.all loop
         PAP.Projects.Clear;
      end loop;
      Deallocate (PAP_Array);
   end Free;

   -----------------------------
   -- From_Aggregated_Subtree --
   -----------------------------

   function From_Aggregated_Subtree
     (Prj : GPR2.Project.View.Object)
      return GPR2.Project.View.Vector.Object
   is
      use GPR2;

      Result : Project.View.Vector.Object;
      Seen   : Project.View.Set.Object;

      procedure Process (Prj : Project.View.Object);
      --  Populates result with views of imported projects and aggregated
      --  libraries.

      -------------
      -- Process --
      -------------

      procedure Process (Prj : Project.View.Object) is
      begin
         if Seen.Contains (Prj) then
            return;
         end if;

         Seen.Include (Prj);
         Result.Append (Prj);

         if Prj.Kind = K_Aggregate_Library then
            for P of Prj.Aggregated loop
               Process (P);
            end loop;
         end if;

         for P of Prj.Imports loop
            Process (P);
         end loop;

         for P of Prj.Limited_Imports loop
            Process (P);
         end loop;

      end Process;

   begin
      Process (Prj);
      return Result;
   end From_Aggregated_Subtree;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : GPR2_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Langkit_Support.Text.Text_Type;
      Kind        : Libadalang.Common.Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class
   is
      use Libadalang.Common;

      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return LAL.Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Langkit_Support.Text.Text_Type :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Langkit_Support.Text.Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            return LAL.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : GPR2_Unit_Provider;
      Name     : Langkit_Support.Text.Text_Type;
      Kind     : Libadalang.Common.Analysis_Unit_Kind) return String
   is
      use GPR2;
      use GPR2.Project;
      use Libadalang.Common;
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR2_Lock.Lock'Access);

      Str_Name : constant Name_Type :=
        Name_Type (Libadalang.Unit_Files.Unit_String_Name (Name));
   begin

      for P of Provider.Projects loop
         for V of From_Aggregated_Subtree (P) loop

            declare
               Inf : constant GPR2.Project.Unit_Info.Object :=
                       V.Unit (Str_Name);
            begin
               if Inf.Is_Defined then
                  case Kind is
                     when Unit_Specification =>
                        if Inf.Has_Spec then
                           return Inf.Spec.Source.Value;
                        end if;
                     when Unit_Body =>
                        if Inf.Has_Body then
                           return Inf.Main_Body.Source.Value;
                        end if;
                  end case;
               end if;
            end;

         end loop;
      end loop;

      if Provider.Tree.Has_Runtime_Project then
         declare
            Inf : constant GPR2.Project.Unit_Info.Object :=
                    Provider.Tree.Runtime_Project.Unit (Str_Name);
         begin
            if Inf.Is_Defined then
               case Kind is
                  when Unit_Specification =>
                     if Inf.Has_Spec then
                        return Inf.Spec.Source.Value;
                     end if;
                  when Unit_Body =>
                     if Inf.Has_Body then
                        return Inf.Main_Body.Source.Value;
                     end if;
               end case;
            end if;
         end;
      end if;

      Trace.Trace ("No " & Kind'Img & " for " & String (Str_Name));

      return "";
   end Get_Unit_Filename;

   ----------------
   -- Part_Image --
   ----------------

   function Part_Image (Part : Aggregate_Part) return String is
      use Ada.Strings.Unbounded;
      Image : Unbounded_String;
      First : Boolean := True;
   begin
      Append (Image, "<");
      for Project of Part.Projects loop
         if First then
            First := False;
         else
            Append (Image, ", ");
         end if;
         Append (Image, String (Project.Name));
      end loop;
      Append (Image, ">");
      return To_String (Image);
   end Part_Image;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out GPR2_Unit_Provider) is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR2_Lock.Lock'Access);
   begin
      Provider.Projects.Clear;
   end Release;

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (FFU  : in out Files_For_Unit;
      File : GPR2.Path_Name.Object;
      Part : GPR2.Unit.Library_Unit_Type)
   is
      Unit_File : constant access US.Unbounded_String :=
        (if Part in GPR2.Unit.Spec_Kind then FFU.Spec_File'Access
           else FFU.Body_File'Access);
   begin
      pragma Assert (Unit_File.all = US.Null_Unbounded_String);
      Unit_File.all :=
        (if File.Is_Defined
         then US.To_Unbounded_String (File.Value)
         else US.Null_Unbounded_String);
   end Set_Unit_File;

   -------------------
   -- Set_Unit_File --
   -------------------

   procedure Set_Unit_File
     (Unit_Files : in out Unit_Files_Maps.Map;
      File       : GPR2.Project.Source.Object)
   is
      use Unit_Files_Maps;

      Unit_Name : constant US.Unbounded_String :=
        US.To_Unbounded_String (String (File.Unit_Name));
      Unit_Part : constant GPR2.Unit.Library_Unit_Type := File.Kind;

      Pos       : Cursor := Unit_Files.Find (Unit_Name);
      Inserted  : Boolean;
   begin
      if not Has_Element (Pos) then
         Unit_Files.Insert (Unit_Name, Pos, Inserted);
         pragma Assert (Inserted);
      end if;

      Set_Unit_File (Unit_Files.Reference (Pos), File.Path_Name, Unit_Part);
   end Set_Unit_File;

   ---------------
   -- Try_Merge --
   ---------------

   function Try_Merge
     (Part       : in out Aggregate_Part;
      Project    : GPR2.Project.View.Object;
      Unit_Files : in out Unit_Files_Maps.Map) return Boolean
   is
      use Unit_Files_Maps;
   begin
      --  If Part contains nothing yet, no need to do the costly overlap check:
      --  just move info there and return.

      if Part.Unit_Files.Is_Empty then
         Part.Projects.Append (Project);
         Part.Unit_Files.Move (Unit_Files);
         return True;
      end if;

      --  Otherwise, first check that Part.Unit_Files and Unit_Files don't have
      --  conflicting units.

      for Prj_Pos in Unit_Files.Iterate loop
         declare
            use Ada.Strings.Unbounded;
            Unit_Name : constant Unbounded_String := Key (Prj_Pos);
            Part_Pos  : constant Cursor := Part.Unit_Files.Find (Unit_Name);
         begin
            if Has_Element (Part_Pos)
               and then Unit_Files.Reference (Prj_Pos).Element.all
                        /= Part.Unit_Files.Reference (Part_Pos).Element.all
            then
               if Trace.Is_Active then
                  Trace.Trace
                    ("Found conflicting source files for unit "
                     & To_String (Unit_Name) & " in " & String (Project.Name)
                     & " and " & Part_Image (Part));
               end if;
               return False;
            end if;
         end;
      end loop;

      --  Finally merge Project and Unit_Files into Part

      Part.Projects.Append (Project);
      for Prj_Pos in Unit_Files.Iterate loop
         declare
            Dummy_Cursor   : Cursor;
            Dummy_Inserted : Boolean;
         begin
            Part.Unit_Files.Insert
              (Key (Prj_Pos), Element (Prj_Pos), Dummy_Cursor, Dummy_Inserted);
         end;
      end loop;

      return True;
   end Try_Merge;

end Libadalang.GPR2_Provider;
