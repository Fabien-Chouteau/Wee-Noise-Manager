-------------------------------------------------------------------------------
--                                                                           --
--                             Wee Noise Manager                             --
--                                                                           --
--                     Copyright (C) 2018 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Manager is free software: you can redistribute it and/or     --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Manager is distributed in the hope that it will be useful,   --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Wee Noise Manager.                                          --
--    If not, see <http://www.gnu.org/licenses/>.                            --
--                                                                           --
-------------------------------------------------------------------------------

with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Combo_Box; use Gtk.Combo_Box;

package body GUI.Pitch_Select is

   procedure On_Change (W : access Gtk_Combo_Box_Record'Class);

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (W : access Gtk_Combo_Box_Record'Class) is
      Self   : constant Widget := Widget (W);
   begin
      Self.Val := Param_Value (Self.Get_Active);
   end On_Change;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id;
                      Step  : Step_Id;
                      Param : Param_Id)
   is
   begin
      Self := new Widget_Record;
      Pitch_Select.Initialize (Self, Track, Step, Param);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Step  : Step_Id;
                         Param : Param_Id)
   is
   begin
      Parent_Package.Initialize (Parent (Self));

      Self.On_Changed (On_Change'Access);

      Self.Track := Track;
      Self.Step  := Step;
      Self.Param := Param;
      Self.Val   := 127 / 4;

      for Val in Param_Value loop
         Self.Insert_Text (Gint (Val), Val'Img);
      end loop;

      Self.Set_Active (Gint (Self.Val));
   end Initialize;

   -----------
   -- Value --
   -----------

   function Value (Self : Widget_Record) return Param_Value
   is (Self.Val);

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Pitch_Select;
