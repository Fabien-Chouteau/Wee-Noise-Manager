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

with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;
with Gtk.Widget;    use Gtk.Widget;

package body GUI.Root_Widget is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget)
   is
   begin
      Self := new Widget_Record;
      Root_Widget.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class)
   is
   begin
      Initialize (Parent (Self));

      GUI.Samples.Gtk_New (Self.Samples);
      Self.Append_Page (Self.Samples, Gtk.Label.Gtk_Label_New ("Samples"));
      GUI.Projects.Gtk_New (Self.Projects);
      Self.Append_Page (Self.Projects, Gtk.Label.Gtk_Label_New ("Projects"));

   end Initialize;
   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      Self.Projects.Update;
   end Update;

end GUI.Root_Widget;
