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
package Analytical_Engine.Mill is

   type Axis is
     (Ingress,
      Ingress_Primed,
      Egress,
      Egress_Primed);

   type Operation_Base is
     (None,
      Add,
      Subtract,
      Multiply,
      Divide);
   subtype Operation is Operation_Base range Add .. Operation_Base'Last;

   type Step is (Up, Down);

   Mill_Error : exception;

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
   is tagged limited private;
   type Instance_P is access all Instance;

   procedure Set_Operation (This : in out Instance; To : Operation);

   procedure Set_Ingress (This : in out Instance; To : Big_Integer);

   procedure Set_Ingress_Primed (This : in out Instance; To : Big_Integer);

   procedure Get_Egress (This : Instance; Result : out Big_Integer);

   procedure Get_Egress_Primed (This : Instance; Result : out Big_Integer);

   function Run_Up_Set (This : Instance) return Boolean;
   --  Run_Up is set False when an operation starts, and set True if
   --  the operation overflows or (in the case of addition or
   --  subtraction) produces a result whose sign is different from
   --  that of the first argument.

   procedure Step_Axes (This : in out Instance;
                        Direction : Step;
                        Amount : Positive);
   --  If Direction is Up (card <amount), the ingress axes are scaled
   --  up by 10**amount (so you need to do this for a division before
   --  the divisor is supplied). If direction is Down (card >amount),
   --  the egress axes are scaled down by 10**amount (so you need to
   --  do this for a multiplication after the multiplier is supplied).

private

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record
        Op             : Operation_Base := None;
        Ingress_Valid  : Boolean        := False;
        Ingress        : Big_Integer;
        Ingress_Primed : Big_Integer;
        Egress         : Big_Integer;
        Egress_Primed  : Big_Integer;
        Run_Up         : Boolean        := False;
     end record;

   overriding
   procedure Initialize (This : in out Instance);

   function Run_Up_Set (This : Instance) return Boolean is (This.Run_Up);

end Analytical_Engine.Mill;
