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

package body Audio_Interface is

   function Alsa_Init return Interfaces.C.int;
   pragma Import (C, Alsa_Init, "alsa_init");

   function Alsa_Close return Interfaces.C.int;
   pragma Import (C, Alsa_Close, "alsa_close");
   pragma Unreferenced (Alsa_Close);

   function Alsa_Send (Buff   : System.Address;
                       Frames : Interfaces.C.unsigned)
                       return Interfaces.C.int;
   pragma Import (C, Alsa_Send, "alsa_send");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Alsa_Init /= 0 then
         raise Program_Error with "Alsa init error";
      end if;

   end Initialize;

   ----------
   -- Send --
   ----------

   procedure Send (Buffer : Stereo_Buffer) is
   begin
      if Alsa_Send (Buffer'Address, Buffer'Length) /= 0 then
         raise Program_Error with "Alsa send error";
      end if;
   end Send;

end Audio_Interface;
