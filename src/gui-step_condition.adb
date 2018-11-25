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

with Gtk.Combo_Box;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;

package body GUI.Step_Condition is

   procedure On_Changed (W : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class);

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (W : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class)
   is
      Self   : constant Widget := Widget (W);
   begin
      Self.Val := Step_Condition_Kind'Val (Self.Get_Active);
   end On_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id;
                      Step  : Step_Id)
   is
   begin
      Self := new Widget_Record;
      Step_Condition.Initialize (Self, Track, Step);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Step  : Step_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;
      Self.Step  := Step;
      Self.Val   := Step_Condition_Kind'First;

      for Kind in Step_Condition_Kind loop
         Self.Insert_Text (Gint (Step_Condition_Kind'Pos (Kind)), Img (Kind));
      end loop;

      Self.Set_Active
        (Gint (Step_Condition_Kind'Pos (Step_Condition_Kind'First)));

      Self.On_Changed (On_Changed'Access);
   end Initialize;

   -----------
   -- Value --
   -----------

   function Value (Self : Widget_Record) return Step_Condition_Kind
   is (Self.Val);

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Step_Condition;
