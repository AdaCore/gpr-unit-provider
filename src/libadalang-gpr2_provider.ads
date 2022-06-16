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

with GPR2;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Vector;


with GNATCOLL.Traces; use GNATCOLL.Traces;

with Libadalang.Analysis;

--  This package provides an ``Unit_Provider`` implementation that relies on
--  GPR2 project library.

package Libadalang.GPR2_Provider is

   package LAL renames Libadalang.Analysis;

   Trace : constant GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LIBADALANG.GPR2_PROVIDER", GNATCOLL.Traces.From_Config);

   type Provider_And_Projects is record
      Provider : LAL.Unit_Provider_Reference;
      Projects : GPR2.Project.View.Vector.Object;
   end record;
 --  Associates one project unit provider with all the projects on which it
 --  has visibility.

   type Provider_And_Projects_Array is
      array (Positive range <>) of Provider_And_Projects;
   type Provider_And_Projects_Array_Access is
      access all Provider_And_Projects_Array;
   procedure Free (PAP_Array : in out Provider_And_Projects_Array_Access);

   function Create_Project_Unit_Providers
     (Tree : GPR2.Project.Tree.Object)
      return Provider_And_Projects_Array_Access;
   --  Create unit providers for consistent sets of projects in ``Tree``.
   --
   --  As unit providers must guarantee that there exists at most one source
   --  file for each couple (unit name, unit kind), this creates more than one
   --  unit providers when Project is an aggregate project that contains
   --  multiple definitions for the same unit.
   --
   --  The project pointed to by ``Tree`` must outlive the returned unit file
   --  providers, and it is up to callers to deallocate ``Tree`` itself.
   --
   --  In order for unit names to be resolved the sources of project tree
   --  need to be computed, so GPR2.Tree.Update_Sources needs to be called
   --  before creating a unit provider. In addition, in order to resolve
   --  runtime unit names corresponding project tree sources need to be updated
   --  with runtime files included: With_Runtime parameter of Update_Sources
   --  needs to be set to True. Source recomputation is a costly operation and
   --  unit provider does not enforce it to avoid unnecessary recomputation.

   Unsupported_View_Error : exception;
   --  See the ``Create_Project_Unit_Provider`` function

   function Create_Project_Unit_Provider
     (Tree : GPR2.Project.Tree.Object;
      View : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return LAL.Unit_Provider_Reference;
   --  Likewise, but create only one unit provider.
   --
   --  If a non-null ``Project`` is given, use it to provide units. Raise an
   --  ``Unsupported_View_Error`` exception if that project aggregates more
   --  than one project in its closure.
   --
   --  If Project is not provided, run ``Create_Project_Unit_Providers``:
   --  if it returns only one provider, return it, otherwise raise an
   --  ``Unsupported_View_Error`` exception.
   --
   --  Tree must outlive the returned unit file provider and project tree
   --  sources must be initialized (see ``Create_Project_Unit_Providers``).

end Libadalang.GPR2_Provider;
