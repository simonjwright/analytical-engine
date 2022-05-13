private package Analytical_Engine.Card.Action_Card is

   pragma Elaborate_Body;

   type Action_Kind is (Ring_Bell, Halt_Engine, Print_Last_Result);

   type Action_Card (Act : Action_Kind) is new Card with record
      case Act is
         when Halt_Engine =>
            Msg : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         when others =>
            null;
      end case;
   end record;
   overriding
   procedure Execute (C : Action_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Action_Card;

end Analytical_Engine.Card.Action_Card;
