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

with Gtk.Notebook;

with Project_Manager; use Project_Manager;

private with GUI.Track_Params;
private with GUI.Track_Pattern;

package GUI.Track_Sequence is

   --  Contains both the pattern and parameters for a track

   subtype Parent_Record is Gtk.Notebook.Gtk_Notebook_Record;
   subtype Parent is Gtk.Notebook.Gtk_Notebook;

   type Widget_Record is new Parent_Record
     and Updatable_Record
     and Reconfigurable_Record
   with private;

   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id);

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id);

   overriding
   procedure Update (Self : in out Widget_Record);

   overriding
   procedure Reconfigure (Self : in out Widget_Record);

private

   type Widget_Record is new Parent_Record
     and Updatable_Record
     and Reconfigurable_Record
   with record
      Track : Track_Id;

      Params  : GUI.Track_Params.Widget;
      Pattern : GUI.Track_Pattern.Widget;
   end record;

end GUI.Track_Sequence;
