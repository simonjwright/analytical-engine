with Analytical_Engine.Mill;

private package Analytical_Engine.Card.Operation_Card is

   pragma Elaborate_Body;

   type Operation_Card is new Card with record
      Op : Mill.Operation;
   end record;
   overriding
   procedure Execute (C : Operation_Card;
                      In_The_Framework : in out Framework.Instance);

   subtype Card is Operation_Card;

end Analytical_Engine.Card.Operation_Card;
