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

package body Analytical_Engine.Mill is

   Max_Value : constant Big_Integer
     := Make ("100000000000000000000000000000000000000000000000000");
   Min_Value : constant Big_Integer := -Max_Value;
   Zero : constant Big_Integer := Make ("0");

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Conversions.To_Wide_String;

   procedure Clear_Ingress (This : in out Instance'Class);
   procedure Clear_Egress (This : in out Instance'Class);

   procedure Set_Operation (This : in out Instance; To : Operation)
   is
   begin
      This.Op := To;
   end Set_Operation;

   procedure Set_Ingress (This : in out Instance; To : Big_Integer)
   is
   begin
      if This.Ingress_Valid then
         This.Run_Up := False;
         case This.Op is
            when None =>
               raise Mill_Error with "operation not set";
            when Add =>
               Add (This.Egress, This.Ingress, To);
               if This.Egress >= Max_Value then
                  This.Run_Up := True;
                  Subtract (This.Egress, This.Egress, Max_Value);
               elsif This.Egress <= Min_Value then
                  This.Run_Up := True;
                  Add (This.Egress, This.Egress, Max_Value);
               elsif (Sign (This.Ingress) >= 0 and Sign (This.Egress) < 0)
                 or else (Sign (This.Ingress) < 0 and Sign (This.Egress) >= 0)
               then
                  This.Run_Up := True;
               end if;
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (This.Ingress)
                         & " + "
                         & Image (To)
                         & " = "
                         & Image (This.Egress)
                         & (if This.Run_Up then ", run_up" else "")));
               end if;
            when Subtract =>
               Subtract (This.Egress, This.Ingress, To);
               if This.Egress >= Max_Value then
                  This.Run_Up := True;
                  Subtract (This.Egress, This.Egress, Max_Value);
               elsif This.Egress <= Min_Value then
                  This.Run_Up := True;
                  Add (This.Egress, This.Egress, Max_Value);
               elsif (Sign (This.Ingress) >= 0 and Sign (This.Egress) < 0)
                 or else (Sign (This.Ingress) < 0 and Sign (This.Egress) >= 0)
               then
                  This.Run_Up := True;
               end if;
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (This.Ingress)
                         & " - "
                         & Image (To)
                         & " = "
                         & Image (This.Egress)
                         & (if This.Run_Up then ", run_up" else "")));
               end if;
            when Multiply =>
               Multiply (This.Egress, This.Ingress, To);
               Divide (This.Egress_Primed, This.Egress, Max_Value);
               Get_Rem (This.Egress, This.Egress, Max_Value);
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (This.Ingress)
                         & " * "
                         & Image (To)
                         & " = "
                         & Image (This.Egress_Primed)
                         & ","
                         & Image (This.Egress)));
               end if;
            when Divide =>
               if Sign (To) = 0 then
                  This.Run_Up := True;  -- egress axes left at zero
               else
                  Multiply (This.Egress, This.Ingress_Primed, Max_Value);
                  Add (This.Egress, This.Egress, This.Ingress);
                  Divide (This.Egress_Primed, This.Egress, To);
                  if This.Egress_Primed >= Max_Value
                    or else This.Egress_Primed <= Min_Value
                  then
                     --  The Java code resets both egress axes; would
                     --  this have been Babbage's solution? or would
                     --  it have been "undefined behaviour"?
                     This.Clear_Egress;
                     This.Run_Up := True;
                  else
                     Get_Rem (This.Egress, This.Egress, To);
                  end if;
               end if;
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (This.Ingress)
                         & " / "
                         & Image (To)
                         & " = "
                         & Image (This.Egress_Primed)
                         & ", rem "
                         & Image (This.Egress)
                         & (if This.Run_Up then ", run_up" else "")));
               end if;
         end case;
         Clear_Ingress (This);
      else
         This.Clear_Egress;
         Set (This.Ingress, To);
         This.Ingress_Valid := True;
      end if;
   end Set_Ingress;

   procedure Set_Ingress_Primed (This : in out Instance; To : Big_Integer)
   is
   begin
      Set (This.Ingress_Primed, To);
   end Set_Ingress_Primed;

   procedure Get_Egress (This : Instance; Result : out Big_Integer)
   is
   begin
      Set (Result, This.Egress);
   end Get_Egress;

   procedure Get_Egress_Primed (This : Instance; Result : out Big_Integer)
   is
   begin
      Set (Result, This.Egress_Primed);
   end Get_Egress_Primed;

   procedure Step_Axes (This : in out Instance;
                        Direction : Step;
                        Amount : Positive)
   is
      Ten : constant Big_Integer := Make ("10");
      Scale : Big_Integer := Make ("1");
   begin
      for J in 1 .. Amount loop
         Multiply (Scale, Scale, Ten);
      end loop;
      case Direction is
         when Up =>
            if This.Op /= Divide then
               raise Mill_Error with "Step up when operation not /";
            end if;
            --  Scale the ingress axes
            declare
               Tmp : Big_Integer;
            begin
               Multiply (Tmp, This.Ingress_Primed, Max_Value);
               Add (Tmp, Tmp, This.Ingress);
               Multiply (Tmp, Tmp, Scale);
               Divide (This.Ingress_Primed, Tmp, Max_Value);
               Get_Rem (This.Ingress, Tmp, Max_Value);
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (Tmp)
                         & " <"
                         & Amount'Img
                         & " = "
                         & Image (This.Ingress_Primed)
                         & "," & Image (This.Ingress)));
               end if;
            end;
         when Down =>
            --  Scale the egress axes
            if This.Op /= Multiply then
               raise Mill_Error with "Step down when operation not *";
            end if;
            declare
               Input : Big_Integer;
               Result : Big_Integer;
            begin
               Multiply (Input, This.Egress_Primed, Max_Value);
               Add (Input, Input, This.Egress);
               Divide (Result, Input, Scale);
               Divide (This.Egress_Primed, Result, Max_Value);
               Get_Rem (This.Egress, Result, Max_Value);
               if This.Panel.Tracing then
                  This.Panel.Log_Trace_Message
                    (+("Mill: "
                         & Image (Input)
                         & " >"
                         & Amount'Img
                         & " = "
                         & Image (This.Egress_Primed)
                         & "," & Image (This.Egress)));
               end if;
            end;
      end case;
   end Step_Axes;

   procedure Initialize (This : in out Instance)
   is
   begin
      This.Op := None;
      This.Ingress_Valid := False;
      This.Run_Up := False;
      Set (This.Ingress, Zero);
      Set (This.Ingress_Primed, Zero);
      Set (This.Egress, Zero);
      Set (This.Egress_Primed, Zero);
   end Initialize;

   procedure Clear_Ingress (This : in out Instance'Class)
   is
   begin
      This.Ingress_Valid := False;
      Set (This.Ingress, Zero);
      Set (This.Ingress_Primed, Zero);
   end Clear_Ingress;

   procedure Clear_Egress (This : in out Instance'Class)
   is
   begin
      Set (This.Egress, Zero);
      Set (This.Egress_Primed, Zero);
   end Clear_Egress;

end Analytical_Engine.Mill;
