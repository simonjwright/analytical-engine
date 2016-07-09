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

with Ada.Strings.Unbounded;

limited with Analytical_Engine.Framework;

private with Ada.Finalization;
private with Ada.Strings.Maps;
private with Analytical_Engine.Mill;
private with Analytical_Engine.Store;
private with GNATCOLL.GMP.Integers;
private with System;

package Analytical_Engine.Card is

   --  Notes from http://www.fourmilab.ch/babbage/cards.html.

   --  Program Cards

   --  A program for The Analytical Engine is composed of a chain of
   --  cards of different varieties and content. In our emulation of
   --  the Engine, the chain is represented by a series of lines in a
   --  text file, one card per line. To run a calculation on the
   --  Engine, the card chain prepared by the analyst is submitted to
   --  the Engine's human attendant, who examines it for possible
   --  errors and requests for actions by the Attendant (for example,
   --  to include the cards for a previously-prepared standard
   --  computation such as the extraction of the square root of a
   --  number at a certain point in the submitted chain).

   --  After completing all requests for attendant assistance in
   --  preparing the chain and determining that it is free of obvious
   --  errors, the attendant mounts the chain on the Card Reader and
   --  causes the Engine to begin to process it. The operation of the
   --  Engine may be halted by reaching the end of the chain, by a
   --  card which directs the Engine to stop, or by a card which
   --  pauses the processing of the Engine and requests (by a visible
   --  annotation written on the card) the attendant to take some
   --  action and then resume the operation of the Engine.

   type Card is abstract tagged record
      Line_Number : Natural := 0;
      Source_File : Ada.Strings.Unbounded.Unbounded_String;
      Source      : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   type Card_P is access all Card'Class;

   procedure Execute
     (C : Card;
      In_The_Framework : in out Analytical_Engine.Framework.Instance)
     is abstract;
   function Equals (L, R : Card'Class) return Boolean;

   Card_Error : exception;

   function Read (From : String) return Card'Class;

private

   use Ada.Strings.Unbounded;
   use GNATCOLL.GMP.Integers;

   function Equals (L, R : Card'Class) return Boolean is
     (System."=" (L'Address, R'Address));

   White_Space : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & ASCII.HT);
   White_Space_Or_Plus : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" +" & ASCII.HT);

   type Big_Integer_P is access Big_Integer; -- Big_Integer is limited
   type Controlled_Big_Integer is new Ada.Finalization.Controlled with record
      Number : Big_Integer_P;
   end record;
   overriding
   procedure Adjust (Obj : in out Controlled_Big_Integer); -- deep copy
   overriding
   procedure Finalize (Obj : in out Controlled_Big_Integer);

   type Number_Card is new Card with record
      Target_Column : Store.Column;
      Value         : Controlled_Big_Integer;
   end record;
   overriding
   procedure Execute (C : Number_Card;
                      In_The_Framework : in out Framework.Instance);

   type Operation_Card is new Card with record
      Op : Mill.Operation;
   end record;
   overriding
   procedure Execute (C : Operation_Card;
                      In_The_Framework : in out Framework.Instance);

   type Variable_Card is new Card with record
      Axis     : Mill.Axis; -- Ingress axes are to mill, egress to store
      Column   : Store.Column;
      Preserve : Boolean;   -- If false and the value is being sent to
                            -- the mill, the source column is reset to
                            -- zero after the value has been
                            -- retrieved.
   end record;
   overriding
   procedure Execute (C : Variable_Card;
                      In_The_Framework : in out Framework.Instance);

   type Stepping_Card is new Card with record
      Direction  : Mill.Step;
      Step_Count : Positive;
   end record;
   overriding
   procedure Execute (C : Stepping_Card;
                      In_The_Framework : in out Framework.Instance);

   type Combinatorial_Card is new Card with record
      Advance     : Boolean;
      Conditional : Boolean;
      Card_Count  : Positive;
   end record;
   overriding
   procedure Execute (C : Combinatorial_Card;
                      In_The_Framework : in out Framework.Instance);

   type Action_Kind is (Ring_Bell, Halt_Engine, Print_Last_Result);
   type Action_Card (Act : Action_Kind) is new Card with record
      case Act is
         when Halt_Engine =>
            Msg : Ada.Strings.Unbounded.Unbounded_String;
         when others =>
            null;
      end case;
   end record;
   overriding
   procedure Execute (C : Action_Card;
                      In_The_Framework : in out Framework.Instance);

   type Comment_Card is new Card with null record;
   overriding
   procedure Execute (C : Comment_Card;
                      In_The_Framework : in out Framework.Instance);

   type Tracing_Card is new Card with record
      Tracing : Boolean;
   end record;
   overriding
   procedure Execute (C : Tracing_Card;
                      In_The_Framework : in out Framework.Instance);

   --  type Curve_Drawing_Card is new Card with private;

   --  Attendant cards: see Analytical_Engine.Card.Attendant_Request.

end Analytical_Engine.Card;
