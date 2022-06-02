--  Test that Libadalang's project unit provider behaves as expected. First
--  check that unsupported projects are properly rejected, then load a
--  supported one an check that name resolution properly uses it.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Context;
with GPR2.Project.View; use GPR2.Project.View;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.GPR2_Provider; use Libadalang.GPR2_Provider;

with GNAT.Traceback.Symbolic;

procedure Main is

   Tree : GPR2.Project.Tree.Object;

   function Load_Project
     (File : GPR2.Filename_Type; Project : GPR2.Optional_Name_Type := "")
      return Unit_Provider_Reference;
   procedure Try_Loading_Project (File : String; Project : String := "");

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (File : GPR2.Filename_Type; Project : GPR2.Optional_Name_Type := "")
      return Unit_Provider_Reference
   is
      use type GPR2.Optional_Name_Type;
      Prj  : GPR2.Project.View.Object := Undefined;
   begin
      Put_Line ("Loading " & String (File) & "...");
      if Project'Length > 0 then
         Put_Line ("   Targetting subproject " & String (Project));
      end if;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File (File),
         Context  => GPR2.Context.Empty);
      Tree.Update_Sources;
      if Project'Length > 0 then
         for V of Tree.Ordered_Views loop
            if V.Name = Project then
               Prj := V;
               exit;
            end if;
         end loop;

         pragma Assert (Prj.Is_Defined);
      end if;
      return Create_Project_Unit_Provider (Tree, Prj);
   exception
      when others =>
         Prj := Undefined;
         Tree.Unload;
         raise;
   end Load_Project;

   -------------------------
   -- Try_Loading_Project --
   -------------------------

   procedure Try_Loading_Project (File : String; Project : String := "") is
      Dummy : Unit_Provider_Reference;
   begin
      Dummy := Load_Project
        (GPR2.Filename_Type (File),
         GPR2.Optional_Name_Type (Project));
      Tree.Unload;
      Put_Line ("   Success");
   exception
      when Exc : GPR2.Project_Error =>
         Put_Line ("   Invalid_Project exception: "
                   & Ada.Exceptions.Exception_Message (Exc));
      when Exc : Unsupported_View_Error =>
         Put_Line ("   Unsupported_View_Error exception: "
                   & Ada.Exceptions.Exception_Message (Exc));
   end Try_Loading_Project;

begin
   Try_Loading_Project ("unsupported_aggr.gpr");
   Try_Loading_Project ("unsupported_aggr.gpr", "unsupported_aggr");
   Try_Loading_Project ("unsupported_aggr.gpr", "p");
   Try_Loading_Project ("supported_no_conflict.gpr");
   Try_Loading_Project ("supported_simple_aggr.gpr");
   Try_Loading_Project ("supported_simple_aggr.gpr", "supported_simple_aggr");
   Try_Loading_Project ("supported_chained_aggr.gpr");
   Try_Loading_Project ("supported_chained_aggr.gpr",
                        "supported_chained_aggr");

   declare
      Ctx  : constant Analysis_Context :=
         Create_Context (Unit_Provider => Load_Project ("p.gpr"));
      Unit : constant Analysis_Unit :=
         Get_From_Provider (Ctx, "p2", Unit_Specification);
      Root : constant Ada_Node := Unit.Root;

      Subtype_Ind : constant Subtype_Indication := Find_First
        (Root, Kind_Is (Ada_Subtype_Indication)).As_Subtype_Indication;
      Res_Type    : constant Ada_Node_Array :=
         Subtype_Ind.F_Name.P_Matching_Nodes;
   begin
      Put_Line (Subtype_Ind.Image & " resolves to:");
      for E of Res_Type loop
         Put_Line ("  " & E.Image);
      end loop;
   end;

   Put_Line ("Done.");

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E)
                & ": "
                & Ada.Exceptions.Exception_Message (E)
                & ASCII.LF
                & GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Main;
