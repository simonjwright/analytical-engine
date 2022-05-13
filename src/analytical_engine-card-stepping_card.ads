with Analytical_Engine.Mill;

private package Analytical_Engine.Card.Stepping_Card is

   pragma Elaborate_Body;

   type Stepping_Card is new Card with record
      Direction  : Mill.Step;
      Step_Count : Positive;
   end record;
   overriding
   procedure Execute (C : Stepping_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Stepping_Card;

end Analytical_Engine.Card.Stepping_Card;
