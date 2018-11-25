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

with Gtk.Event_Box; use Gtk.Event_Box;
with Gdk.Event;
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;
with Cairo;         use Cairo;

package body GUI.Signal_Path is

   function On_Press (W     : access Gtk_Widget_Record'Class;
                      Event : Gdk.Event.Gdk_Event_Button)
                      return Boolean;

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean;

   --------------
   -- On_Press --
   --------------

   function On_Press (W     : access Gtk_Widget_Record'Class;
                      Event : Gdk.Event.Gdk_Event_Button)
                      return Boolean
   is
      pragma Unreferenced (Event);
      Self : constant Widget := Widget (W);
   begin
      Self.To_Main_Mix := not Self.To_Main_Mix;
      Self.Queue_Draw;
      return True;
   end On_Press;

   -------------
   -- On_Draw --
   -------------

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean
   is
      Width  : constant Gdouble := Gdouble (W.Get_Allocated_Width);
      Height : constant Gdouble := Gdouble (W.Get_Allocated_Height);
      Self   : constant Widget := Widget (W);

      Main_Mix_Line   : constant Gdouble := 0.75;
      Audio_Path_Line : constant Gdouble := 0.30;
   begin
      Set_Source_Rgb (Cr,
                      Red   => 1.0,
                      Green => 1.0,
                      Blue  => 1.0);
      Paint (Cr);

      Set_Source_Rgb (Cr,
                      Red   => 0.0,
                      Green => 0.0,
                      Blue  => 0.0);

      Set_Line_Width (Cr, 2.0);

      --  Source to audio path
      Curve_To (Cr,
                0.0,                     0.1 * Height,
                Audio_Path_Line * Width, 0.1 * Height,
                Audio_Path_Line * Width, 0.5 * Height);

      --  Audio path line
      if Self.To_Main_Mix then
         Curve_To (Cr,
                   Audio_Path_Line * Width, 0.50 * Height,
                   Audio_Path_Line * Width, 0.75 * Height,
                     Main_Mix_Line * Width, 0.75 * Height);
      else
         Curve_To (Cr,
                   Audio_Path_Line * Width, 0.50 * Height,
                   Audio_Path_Line * Width, 0.75 * Height,
                   Audio_Path_Line * Width,        Height);
      end if;
      Stroke (Cr);

      --  main mix path line
      Move_To (Cr, Main_Mix_Line * Width, 0.0);
      Line_To (Cr, Main_Mix_Line * Width, Height);
      Stroke (Cr);

      return True;
   end On_Draw;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Signal_Path.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;

      Self.Set_Size_Request (50, -1);

      Self.Set_Events (Gdk.Event.Button_Press_Mask);
      Self.On_Button_Press_Event (On_Press'Access);
      Self.On_Draw (On_Draw'Access, After => True);
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Widget_Record) is
   begin
      raise Program_Error;
   end Update;

end GUI.Signal_Path;
