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

with Gtk.Grid;      use Gtk.Grid;
with Gtk.Label;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Enums;
with Gtk.Button;

package body GUI.Track_Config is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Config.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
      Label : Gtk.Label.Gtk_Label;
   begin
      Initialize (Parent (Self));

      Self.Track := Track;

      GUI.Track_Engine_Select.Gtk_New (Self.Engine, Track);
      GUI.Track_Sub_Select.Gtk_New (Self.Sub, Track);

      Gtk.Label.Gtk_New (Label, Track'Img);
      Label.Set_Justify (Gtk.Enums.Justify_Center);
      Label.Set_Width_Chars (3);

      GUI.Signal_Path.Gtk_New (Self.Signal, Track);

      Self.Attach (Self.Engine, 0, 0, 3, 1);
      Self.Attach (Self.Sub, 0, 1, 3, 1);
      Self.Attach (Label, 0, 2, 1, 1);
      Self.Attach (Self.Signal, 3, 0, 1, 3);
      Self.Attach (Gtk.Button.Gtk_Button_New_With_Label ("M"), 1, 2, 1, 1);
      Self.Attach (Gtk.Button.Gtk_Button_New_With_Label ("S"), 2, 2, 1, 1);
   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      null;
   end Update;

end GUI.Track_Config;
