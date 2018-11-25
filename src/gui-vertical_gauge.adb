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

package body GUI.Vertical_Gauge is

   function On_Motion (W     : access Gtk_Widget_Record'Class;
                       Event : Gdk.Event.Gdk_Event_Motion)
                       return Boolean;

   function On_Draw (W  : access Gtk_Widget_Record'Class;
                     Cr : Cairo.Cairo_Context)
                     return Boolean;

   ---------------
   -- On_Motion --
   ---------------

   function On_Motion (W  : access Gtk_Widget_Record'Class;
                       Event : Gdk.Event.Gdk_Event_Motion)
                       return Boolean
   is
      H    : constant Gdouble := Gdouble (W.Get_Allocated_Height);
      Y    : constant Gdouble := Event.Y;
      Self : constant Widget := Widget (W);
   begin
      if Y < H then
         Self.Val := 127 - Param_Value ((Y / H) * 127.0);
      end if;

      W.Queue_Draw;

      return False;
   end On_Motion;

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
      Val    : constant Gdouble :=
        Gdouble (Self.Val) / Gdouble (Param_Value'Last);
   begin
      Set_Source_Rgb (Cr, Red   => 1.0,
                         Green => 1.0,
                         Blue  => 1.0);
      Paint (Cr);

      Set_Source_Rgb (Cr, Red   => 0.0,
                      Green => 0.0,
                      Blue  => 0.0);

      if Self.Fill then
         Move_To (Cr, 0.0, Height * (1.0 - Val));
         Line_To (Cr, Width, Height * (1.0 - Val));
         Line_To (Cr, Width, Height);
         Line_To (Cr, 0.0, Height);
         Fill (Cr);
      else
         Set_Line_Width (Cr, 2.0);

         Move_To (Cr, 0.0, Height * (1.0 - Val));
         Line_To (Cr, Width, Height * (1.0 - Val));
         Stroke (Cr);
      end if;
      return True;
   end On_Draw;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id;
                      Step  : Step_Id;
                      Param : Param_Id;
                      Fill  : Boolean := False)
   is
   begin
      Self := new Widget_Record;
      Vertical_Gauge.Initialize (Self, Track, Step, Param, Fill);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Step  : Step_Id;
                         Param : Param_Id;
                         Fill  : Boolean := False)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;
      Self.Step  := Step;
      Self.Param := Param;
      Self.Fill  := Fill;
      Self.Val   := 127 / 4;

      Self.Set_Events (Gdk.Event.Pointer_Motion_Mask);
      Self.On_Motion_Notify_Event (On_Motion'Access, After => True);
      Self.On_Draw (On_Draw'Access, After => True);

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

end GUI.Vertical_Gauge;
