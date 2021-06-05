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
with Analytical_Engine.Output;
with Analytical_Engine.Attendant;
with Analytical_Engine.Mill;
with Analytical_Engine.Store;
with Analytical_Engine.Card_Reader;

private with Ada.Finalization;

package Analytical_Engine.Framework is

   type Instance (<>) is tagged limited private;

   function Panel (This : Instance)
                  return Analytical_Engine.Annunciator_Panel.Class_P;
   function Attendant (This : Instance)
                      return Analytical_Engine.Attendant.Instance_P;
   function Mill (This : Instance)
                 return Analytical_Engine.Mill.Instance_P;
   function Store (This : Instance)
                  return Analytical_Engine.Store.Instance_P;
   function Card_Reader (This : Instance)
                        return Analytical_Engine.Card_Reader.Instance_P;
   function Output (This : Instance)
                        return Analytical_Engine.Output.Class_P;

   function Create
     (With_Panel  : not null Analytical_Engine.Annunciator_Panel.Class_P;
      With_Output : not null Analytical_Engine.Output.Class_P)
     return Instance;

   procedure Run (This : in out Instance);

private

   type Instance (Panel     : not null Annunciator_Panel.Class_P;
                  Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record

        Mill : Analytical_Engine.Mill.Instance_P;
          --  (Panel     => Panel,
          --   Attendant => Attendant);

        Store : Analytical_Engine.Store.Instance_P;
          --  (Panel     => Panel,
          --   Attendant => Attendant);

        Card_Reader : Analytical_Engine.Card_Reader.Instance_P;
          --  (Panel     => Panel,
          --   Attendant => Attendant);

        Output      : Analytical_Engine.Output.Class_P;

     end record;

   function Panel (This : Instance)
                  return Analytical_Engine.Annunciator_Panel.Class_P is
     (This.Panel);

   function Attendant (This : Instance)
                      return Analytical_Engine.Attendant.Instance_P is
     (This.Attendant);

   function Mill (This : Instance)
                 return Analytical_Engine.Mill.Instance_P is
     (This.Mill);

   function Store (This : Instance)
                  return Analytical_Engine.Store.Instance_P is
     (This.Store);

   function Card_Reader (This : Instance)
                        return Analytical_Engine.Card_Reader.Instance_P is
     (This.Card_Reader);

   function Output (This : Instance)
                   return Analytical_Engine.Output.Class_P is
     (This.Output);

end Analytical_Engine.Framework;
