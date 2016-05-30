package body Analytical_Engine.Framework is

   function Create
     (With_Panel  : not null Analytical_Engine.Annunciator_Panel.Class_P;
      With_Output : not null Analytical_Engine.Output.Class_P)
     return Instance
   is
      With_Attendant : constant Analytical_Engine.Attendant.Instance_P
        := new Analytical_Engine.Attendant.Instance (Panel => With_Panel);
   begin
      return E : Instance (Panel     => With_Panel,
                           Attendant => With_Attendant) do
         E.Mill := new Analytical_Engine.Mill.Instance
           (Panel => With_Panel,
            Attendant => With_Attendant);
         E.Store := new Analytical_Engine.Store.Instance
           (Panel => With_Panel,
            Attendant => With_Attendant);
         E.Card_Reader := new Analytical_Engine.Card_Reader.Instance
           (Panel => With_Panel,
            Attendant => With_Attendant);
         E.Output := With_Output;
      end return;
   end Create;

   procedure Run (This : in out Instance) is
   begin
      This.Panel.Log_Attendant_Message ("running");
      This.Card_Reader.Execute (In_The_Framework => This);
   end Run;

end Analytical_Engine.Framework;
