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

with Ada.Characters.Conversions;
with Ada.Strings.Wide_Unbounded;

with Analytical_Engine.Framework;

with GNAT.Regpat;

package body Analytical_Engine.Card.Attendant_Request is

   Picture_Matcher : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("a\s+write\s+numbers\s+as\s+(.+)$",
                             Flags => GNAT.Regpat.Case_Insensitive);

   Row_Column_Matcher : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("a\s+write\s+in\s(rows|columns)$",
                             Flags => GNAT.Regpat.Case_Insensitive);

   Annotation_Matcher : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("a\s+write\s+annotation (.+)$",
                             Flags => GNAT.Regpat.Case_Insensitive);

   New_Line_Matcher : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("a\s+write\s+new\s+line\s*$",
                             Flags => GNAT.Regpat.Case_Insensitive);

   Max_Parens : constant := 10; -- overkill

   function "+" (Item : Wide_String) return String
     is (Ada.Characters.Conversions.To_String (Item, Substitute => ' '));

   function Read (From : Wide_String) return Card'Class is
      Matches : GNAT.Regpat.Match_Array (0 .. Max_Parens);
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (Picture_Matcher, +From, Matches);
      if Matches (0) /= GNAT.Regpat.No_Match then
         return C : Picture_Card do
            C.Source := To_Unbounded_Wide_String (From);
            C.Picture :=
              To_Unbounded_Wide_String
                (From (Matches (1).First .. Matches (1).Last));
         end return;
      end if;

      GNAT.Regpat.Match (Row_Column_Matcher, +From, Matches);
      if Matches (0) /= GNAT.Regpat.No_Match then
         return C : Row_Column_Card do
            C.Source := To_Unbounded_Wide_String (From);
            C.In_Rows := From (Matches (1).First .. Matches (1).Last) = "rows";
         end return;
      end if;

      GNAT.Regpat.Match (Annotation_Matcher, +From, Matches);
      if Matches (0) /= GNAT.Regpat.No_Match then
         return C : Annotation_Card do
            C.Source := To_Unbounded_Wide_String (From);
            C.Annotation :=
              To_Unbounded_Wide_String
                (From (Matches (1).First .. Matches (1).Last));
         end return;
      end if;

      GNAT.Regpat.Match (New_Line_Matcher, +From, Matches);
      if Matches (0) /= GNAT.Regpat.No_Match then
         return C : New_Line_Card do
            C.Source := To_Unbounded_Wide_String (From);
         end return;
      end if;
      raise Card_Error
        with "unrecognised attendant request card";
   end Read;

   --  type Attendant_Request_Kind is
   --    (Calculation_Trace,
   --     Advancing_Or_Backing_Block,
   --     Alternation,
   --     End_Block,
   --     Library_Inclusion,
   --     Decimal_Place_Expansion,

   procedure Execute (C : Picture_Card;
                      In_The_Framework : in out Framework.Instance) is
   begin
      In_The_Framework.Output.Set_Picture (C.Picture);
   end Execute;

   --     Numeric_Output_Format_With_Decimal_Point,

   procedure Execute (C : Row_Column_Card;
                      In_The_Framework : in out Framework.Instance) is
   begin
      In_The_Framework.Output.Writing_Style (In_Rows => C.In_Rows);
   end Execute;

   procedure Execute (C : Annotation_Card;
                      In_The_Framework : in out Framework.Instance) is
   begin
      In_The_Framework.Output.Output (To_Wide_String (C.Annotation));
   end Execute;

   procedure Execute (C : New_Line_Card;
                      In_The_Framework : in out Framework.Instance) is
      pragma Unreferenced (C);
      use Ada.Characters.Conversions;
   begin
      In_The_Framework.Output.Output ((1 => To_Wide_Character (ASCII.LF)));
   end Execute;

   --     Not_A_Request);

end Analytical_Engine.Card.Attendant_Request;
