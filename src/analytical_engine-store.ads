--  Copyright (C) Simon Wright <simon@pushface.org>
--
--  This file is part of the Analytical Engine Ada emulator
--  project. This file is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This file is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.

with Analytical_Engine.Annunciator_Panel;
with Analytical_Engine.Attendant;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
private with Ada.Finalization;
package Analytical_Engine.Store is

   type Column is range 0 .. 999;

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
   is tagged limited private;
   type Instance_P is access all Instance;

   procedure Set (This : in out Instance; Col : Column; To : Big_Integer);

   procedure Get (This : Instance; Col : Column; Result : out Big_Integer);

private

   type Column_Array is array (Column) of Big_Integer;

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record
        Columns : Column_Array;
     end record;

end Analytical_Engine.Store;
