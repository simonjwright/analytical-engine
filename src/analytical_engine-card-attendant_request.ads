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

--  with Ada.Strings.Unbounded;

--  limited with Analytical_Engine.Framework;

--  private with Ada.Finalization;
--  private with Analytical_Engine.Mill;
--  private with Analytical_Engine.Store;
--  private with GNATCOLL.GMP.Integers;
--  private with System;

private package Analytical_Engine.Card.Attendant_Request is

   function Read (From : Wide_String) return Card'Class
   with Pre => From'Length > 0
               and then (From (From'First) = 'A'
                           or else From (From'First) = 'a');

private

   --  type Attendant_Request_Kind is
   --    (Calculation_Trace,
   --     Advancing_Or_Backing_Block,
   --     Alternation,
   --     End_Block,
   --     Library_Inclusion,
   --     Decimal_Place_Expansion,

   type Picture_Card is new Card with record
      Picture : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   end record;
   overriding
   procedure Execute (C : Picture_Card;
                      In_The_Framework : in out Framework.Instance);

   --     Numeric_Output_Format_With_Decimal_Point,

   type Row_Column_Card is new Card with record
      In_Rows : Boolean;
   end record;
   overriding
   procedure Execute (C : Row_Column_Card;
                      In_The_Framework : in out Framework.Instance);

   type Annotation_Card is new Card with record
      Annotation : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   end record;
   overriding
   procedure Execute (C : Annotation_Card;
                      In_The_Framework : in out Framework.Instance);

   type New_Line_Card is new Card with null record;
   overriding
   procedure Execute (C : New_Line_Card;
                      In_The_Framework : in out Framework.Instance);

   --     Not_A_Request);

   --  type Attendant_Request_T (Kind : Attendant_Request_Kind) is record
   --     case Kind is
   --        when Calculation_Trace =>
   --           Trace : Boolean;
   --        when Advancing_Or_Backing_Block =>
   --           Advancing     : Boolean;
   --           Conditionally : Boolean;
   --           Cards         : Positive;
   --        when Alternation =>
   --           null;
   --        when End_Block =>
   --           End_Advancing : Boolean;
   --        when Library_Inclusion =>
   --           Library : Boolean;
   --           Name    : Unbounded_String;
   --        when Decimal_Place_Expansion =>
   --           Places : Positive;
   --        when Numeric_Output_Format_As_Picture =>
   --           Picture : Unbounded_String;
   --        when Numeric_Output_Format_With_Decimal_Point =>
   --           null;
   --        when Write_In_Rows =>
   --           null;
   --        when Write_In_Columns =>
   --           null;
   --        when Write_Annotation =>
   --           Annotation : Unbounded_String;
   --        when Write_New_Line =>
   --           null;
   --        when Not_A_Request =>
   --           null;
   --     end case;
   --  end record;

end Analytical_Engine.Card.Attendant_Request;
