with Analytical_Engine.Framework;

with GNATCOLL.GMP.Integers;

package body Analytical_Engine.Card.Variable_Card is

   use GNATCOLL.GMP.Integers;

   procedure Execute
     (C : Variable_Card;
      In_The_Framework : in out Analytical_Engine.Framework.Instance)
   is
      Value : Big_Integer;
   begin
      case C.Axis is
         when Mill.Ingress =>
            In_The_Framework.Store.Get (Col      => C.Column,
                                        Result   => Value,
                                        Preserve => C.Preserve);
            In_The_Framework.Mill.Set_Ingress (Value);
         when Mill.Ingress_Primed =>
            In_The_Framework.Store.Get (Col      => C.Column,
                                        Result   => Value,
                                        Preserve => C.Preserve);
            In_The_Framework.Mill.Set_Ingress_Primed (Value);
         when Mill.Egress =>
            In_The_Framework.Mill.Get_Egress (Value);
            In_The_Framework.Store.Set (Col => C.Column, To => Value);
         when Mill.Egress_Primed =>
            In_The_Framework.Mill.Get_Egress_Primed (Value);
            In_The_Framework.Store.Set (Col => C.Column, To => Value);
      end case;
   end Execute;

end Analytical_Engine.Card.Variable_Card;
