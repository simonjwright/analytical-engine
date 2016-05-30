with Ada.Text_IO;
with Analytical_Engine.Annunciator_Panel;
with Analytical_Engine.Attendant;
with Analytical_Engine.Card;

limited with Analytical_Engine.Framework;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
package Analytical_Engine.Card_Reader is

   type Instance
     (Panel     : not null Analytical_Engine.Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
   is tagged limited private;
   type Instance_P is access all Instance;

   Card_Reader_Error : exception;

   procedure Reset (This : out Instance);

   procedure Add_Cards (This : in out Instance; From_File_Named : String);

   procedure Add_Cards (This : in out Instance; From : Ada.Text_IO.File_Type);

   procedure Execute (This             : in out Instance;
                      In_The_Framework : in out Framework.Instance);

   procedure Step (This : in out Instance; By : Integer);

   procedure Halt (This : in out Instance);

private

   package Chains is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Card.Card'Class,
      "="          => Card.Equals,
      Index_Type   => Positive);

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record
        Index  : Positive := 1;
        Halted : Boolean  := True;
        Chain  : Chains.Vector;
     end record;

   overriding
   procedure Initialize (This : in out Instance);

end Analytical_Engine.Card_Reader;
