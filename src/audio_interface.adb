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

with Interfaces.C; use Interfaces.C;
with Interfaces;   use Interfaces;
with System;

with Audio;          use Audio;
with Sample_Manager; use Sample_Manager;

with GNAT.Bounded_Buffers;

with Soundio; use Soundio;
with Ada.Text_IO; use Ada.Text_IO;
with Soundio_Output; use Soundio_Output;
with Sound_Gen_Interfaces; use Sound_Gen_Interfaces;

package body Audio_Interface is

   package Sample_Block_Buffer is new GNAT.Bounded_Buffers (Sample_Block);

   Sample_Preview_FIFO : Sample_Block_Buffer.Bounded_Buffer
     (Natural (Sample_Block_Id'Last),
      System.Priority'Last);

--     function Alsa_Init return Interfaces.C.int;
--     pragma Import (C, Alsa_Init, "alsa_init");
--
--     function Alsa_Close return Interfaces.C.int;
--     pragma Import (C, Alsa_Close, "alsa_close");
--     pragma Unreferenced (Alsa_Close);
--
--     function Alsa_Send (Buff   : System.Address;
--                         Frames : Interfaces.C.unsigned)
--                         return Interfaces.C.int;
--     pragma Import (C, Alsa_Send, "alsa_send");

   task Audio_Task is
      pragma Priority (System.Priority'Last);
   end Audio_Task;

   type Sample_Preview_Generator is new Generator with null record;

   procedure Next_Samples
     (Self : in out Sample_Preview_Generator;
      Buffer : in out Generator_Buffer);
   pragma Inline (Next_Samples);

   procedure Reset (Self : in out Sample_Preview_Generator);

   function Children
     (Self : in out Sample_Preview_Generator)
      return Generator_Array;

   Sample_Preview : aliased Sample_Preview_Generator;

   ------------------
   -- Next_Samples --
   ------------------

   procedure Next_Samples
     (Self : in out Sample_Preview_Generator;
      Buffer : in out Generator_Buffer)
   is
      pragma Unreferenced (Self);
      Preview    : Sample_Block;
   begin
      if Sample_Preview_FIFO.Empty then
         Buffer := (others => 0.0);
         return;
      else
         Put_Line ("Remove block for preview");
         Sample_Preview_FIFO.Remove (Preview);

         for Index in Preview'Range loop
            Buffer (Buffer_Range_Type (Index - 1)) :=
              Float (Preview (Index)) / Float (Mono_Frame'Last);
         end loop;
      end if;
   end Next_Samples;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Sample_Preview_Generator) is
   begin
      null;
   end Reset;

   --------------
   -- Children --
   --------------

   function Children
     (Self : in out Sample_Preview_Generator)
      return Generator_Array
   is
      pragma Unreferenced (Self);
   begin
      return Empty_Generator_Array;
   end Children;

   ----------------
   -- Audio_Task --
   ----------------

   task body Audio_Task is
      IO                   : constant access Soundio.SoundIo := Create;
      Default_Device_Index : int;
      Device               : access SoundIo_Device;
      Out_Stream           : access SoundIo_Out_Stream;
      Unused               : SoundIo_Error;
   begin
      --  SoundIO connection boilerplate
      Unused := Connect (IO);
      Flush_Events (IO);
      Default_Device_Index := Default_Output_Device_Index (IO);
      Device := Get_Output_Device (IO, Default_Device_Index);
      Out_Stream := Outstream_Create (Device);
      Out_Stream.Format := Format_Float32NE;
      Out_Stream.Write_Callback := Soundio_Output.Write_Callback'Access;

      --  Set the main generator for the audio stream.
      Set_Generator (Out_Stream => Out_Stream, G => Sample_Preview'Access);
      Unused := Outstream_Open (Out_Stream);
      Unused := Outstream_Start (Out_Stream);

      Put_Line ("Backend used : " & IO.current_backend'Img);
      Put_Line ("BACKEND = " & IO.current_backend'Image);

      Play (Out_Stream);
      loop
         Wait_Events (IO);
      end loop;

      pragma Warnings (Off, "Unreachable");
      Outstream_Destroy (Out_Stream);
      Device_Unref (Device);
      Destroy (IO);

--        Preview    : Sample_Block;
--        Stereo_Out : Stereo_Buffer (Preview'Range);
--
--        File : Ada.Streams.Stream_IO.File_Type;
--        Stream : Ada.Streams.Stream_IO.Stream_Access;
--     begin
--        Initialize;
--
--        Ada.Streams.Stream_IO.Create (File,
--                                      Ada.Streams.Stream_IO.Out_File,
--                                      "/dev/stdout");
--        Stream := Ada.Streams.Stream_IO.Stream (File);
--
--        loop
--           if Sample_Preview_FIFO.Empty then
--              Preview := (others => 0);
--           else
--              Sample_Preview_FIFO.Remove (Preview);
--           end if;
--
--           for Index in Preview'Range loop
--              Stereo_Out (Index) := (Preview (Index), Preview (Index));
--           end loop;
--
--           Stereo_Buffer'Write (Stream, Stereo_Out);
--           --   Audio_Interface.Send (Stereo_Out);
--        end loop;
   end Audio_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
--        if Alsa_Init /= 0 then
--           raise Program_Error with "Alsa init error";
--        end if;
      null;
   end Initialize;

   ----------
   -- Send --
   ----------

   procedure Send (Buffer : Stereo_Buffer) is
   begin
--        if Alsa_Send (Buffer'Address, Buffer'Length) /= 0 then
--           raise Program_Error with "Alsa send error";
--        end if;
      null;
   end Send;

   -------------------------
   -- Push_Sample_Preview --
   -------------------------

   procedure Push_Sample_Preview (Data : Sample_Block) is
   begin
      Put_Line ("Push block for preview");
      Sample_Preview_FIFO.Insert (Data);
   end Push_Sample_Preview;

   --------------------------
   -- Sample_Preview_Flush --
   --------------------------

   procedure Sample_Preview_Flush is
      Block : Sample_Block;
   begin

      while not Sample_Preview_FIFO.Empty loop
         Sample_Preview_FIFO.Remove (Block);
      end loop;
   end Sample_Preview_Flush;

end Audio_Interface;
