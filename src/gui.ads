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

with Gtk.Widget;

package GUI is

   type Updatable_Record is interface;
   type Updatable is access all Updatable_Record'Class;

   procedure Update (Self : in out Updatable_Record) is abstract;
   --  This primitive is called when the project/track/step has potentially
   --  changed and the widget may have to update its corresponding status.

   type Reconfigurable_Record is interface;
   type Reconfigurable is access all Reconfigurable_Record'Class;

   procedure Reconfigure (Self : in out Reconfigurable_Record) is abstract;
   --  This primitive is called when the track is reconfigured, for instance
   --  when the sound engine is changed.

   generic
      type Widget_Record (<>) is new Gtk.Widget.Gtk_Widget_Record with private;
      type Widget is access all Widget_Record'Class;
   function Widget_From_Child
     (W  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return not null access Widget_Record'Class;
   --  Utility function to find a wigdet from one of its child

end GUI;
