with Analytical_Engine.Framework;

with GNATCOLL.GMP.Integers;

package body Analytical_Engine.Card.Action_Card is

   pragma SPARK_Mode (On);

   use GNATCOLL.GMP.Integers;

   procedure Execute (C : Action_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      case C.Act is
         when Halt_Engine =>
            In_The_Framework.Panel.Log_Attendant_Message
              ("Halt: " & To_Wide_String (C.Msg));
            In_The_Framework.Card_Reader.Halt;
         when Ring_Bell =>
            In_The_Framework.Panel.Log_Attendant_Message ("Ting!");
         when Print_Last_Result =>
            declare
               Result : Big_Integer;
            begin
               In_The_Framework.Mill.Get_Egress (Result);
               In_The_Framework.Output.Output (Result);
            end;
      end case;
   end Execute;

end Analytical_Engine.Card.Action_Card;
