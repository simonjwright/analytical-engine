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

package body Analytical_Engine.Framework is

   function Create
     (With_Panel  : not null Analytical_Engine.Annunciator_Panel.Class_P;
      With_Output : not null Analytical_Engine.Output.Class_P)
     return Instance
   is
      With_Attendant : constant Analytical_Engine.Attendant.Instance_P
        := new Analytical_Engine.Attendant.Instance (Panel => With_Panel);
   begin
      return E : Instance (Panel     => With_Panel,
                           Attendant => With_Attendant) do

         E.Mill := new Analytical_Engine.Mill.Instance
           (Panel     => With_Panel,
            Attendant => With_Attendant);

         E.Store := new Analytical_Engine.Store.Instance
           (Panel     => With_Panel,
            Attendant => With_Attendant);

         E.Card_Reader := new Analytical_Engine.Card_Reader.Instance
           (Panel     => With_Panel,
            Attendant => With_Attendant);

         E.Output := With_Output;
      end return;
   end Create;

   procedure Run (This : in out Instance) is
   begin
      This.Card_Reader.Execute (In_The_Framework => This);
   end Run;

end Analytical_Engine.Framework;
