private with Ada.Finalization;
package Analytical_Engine.Annunciator_Panel is

   type Instance is abstract tagged limited private;
   type Class_P is access all Instance'Class;

   procedure Log_Attendant_Message
     (This : Instance; Msg : String) is abstract;

   procedure Log_Trace_Message
     (This : Instance; Msg : String) is abstract;

   procedure Set_Tracing (This : in out Instance; To : Boolean);

   function Tracing (This : Instance) return Boolean;

private

   type Instance
     is abstract new Ada.Finalization.Limited_Controlled with record
        Tracing : Boolean := False;
     end record;

   function Tracing (This : Instance) return Boolean is (This.Tracing);

end Analytical_Engine.Annunciator_Panel;
