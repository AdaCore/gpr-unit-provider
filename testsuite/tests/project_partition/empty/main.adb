--  Check that the project partitionner works on projects containing C units

with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Context;

with Libadalang.GPR2_Provider; use Libadalang.GPR2_Provider;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   PAPs : Provider_And_Projects_Array_Access;
begin
   Put_Line ("Loading the project:");
   Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File ("ap.gpr"),
         Context  => GPR2.Context.Empty);
   PAPs := Create_Project_Unit_Providers (Tree);
   for I in PAPs'Range loop
      Put ("  *");
      for P of PAPs (I).Projects loop
         Put (" ");
         Put (String (P.Name));
      end loop;
      New_Line;
   end loop;
   Free (PAPs);
   Tree.Unload;
   Put_Line ("Done.");
end Main;
