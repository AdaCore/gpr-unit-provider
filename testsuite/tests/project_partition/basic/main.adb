--  Check how the project provider constructor builds partition of aggregated
--  projects.

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Context;

with GNATCOLL.Traces;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.GPR2_Provider;    use Libadalang.GPR2_Provider;

procedure Main is

   package LAL renames Libadalang.Analysis;

   type Context_Array is array (Positive range <>) of Analysis_Context;

   Tree : GPR2.Project.Tree.Object;
   PAPs : Provider_And_Projects_Array_Access;

begin
   GNATCOLL.Traces.Parse_Config ("LIBADALANG.GPR2_PROVIDER=yes");
   Put_Line ("Loading the project:");
   Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File ("ap1.gpr"),
         Context  => GPR2.Context.Empty);
   PAPs := Create_Project_Unit_Providers (Tree);

   declare
      Contexts : Context_Array (PAPs'Range);
   begin
      for I in PAPs'Range loop
         Contexts (I) := Create_Context (Unit_Provider => PAPs (I).Provider);
         Put ("  *");
         for P of PAPs (I).Projects loop
            Put (" ");
            Put (String (P.Name));
         end loop;
         New_Line;
      end loop;
      New_Line;
      Free (PAPs);

      declare
         Unit : constant Analysis_Unit :=
            Contexts (1).Get_From_Provider ("p2", Unit_Specification);
         Ref  : constant LAL.Name :=
            Find_First (Unit.Root, Kind_Is (Ada_Object_Decl))
            .As_Object_Decl.F_Renaming_Clause.F_Renamed_Object;
         Decl : constant Basic_Decl := Ref.P_Referenced_Decl;
      begin
         Put_Line (Ref.Image & " resolves to " & Decl.Image);
      end;
   end;

   Tree.Unload;
   Put_Line ("Done.");
end Main;
