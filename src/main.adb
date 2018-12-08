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

with Glib; use Glib;
with Gtk.Main;
with Gtk.Window, Gtk.Widget, Gtk.Enums;
use  Gtk.Window, Gtk.Widget, Gtk.Enums;

with Gtk.Css_Provider; use Gtk.Css_Provider;
with Glib.Error; use Glib.Error;
with My_Handlers;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gdk.Screen;

with GUI.Root_Widget;

procedure Main is
   Window   : Gtk_Window;
   Root     : GUI.Root_Widget.Widget;
   Provider : Gtk_Css_Provider;
   Error    : aliased GError;
begin

   Gtk.Main.Init;

   Gtk_New (Provider);
   My_Handlers.Plop.Connect (Provider,
                             Gtk.Css_Provider.Signal_Parsing_Error,
                             My_Handlers.Arf'Access);

   Add_Provider_For_Screen (Gdk.Screen.Get_Default, +Provider, 600);
   if not Provider.Load_From_Path
     ("test.css",
      Error'Access)
   then
      raise Program_Error with Get_Message (Error);
   end if;

   Gtk.Window.Gtk_New (Window);

   Gtk.Window.Set_Title (Window, "Wee Noise Manager");

   GUI.Root_Widget.Gtk_New (Root);
   Gtk.Window.Add (Window, Root);

   Show_All (Window);

   Gtk.Main.Main;
end Main;
