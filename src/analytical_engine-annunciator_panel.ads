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

private with Ada.Finalization;
package Analytical_Engine.Annunciator_Panel is

   type Instance is abstract tagged limited private;
   type Class_P is access all Instance'Class;

   procedure Log_Attendant_Message
     (This : Instance; Msg : String) is abstract;

   procedure Log_Trace_Message
     (This : Instance; Msg : String) is abstract;

   procedure Set_Tracing (This : in out Instance; To : Boolean);

   function Tracing (This : Instance) return Boolean;

private

   type Instance
     is abstract new Ada.Finalization.Limited_Controlled with record
        Tracing : Boolean := False;
     end record;

   function Tracing (This : Instance) return Boolean is (This.Tracing);

end Analytical_Engine.Annunciator_Panel;
