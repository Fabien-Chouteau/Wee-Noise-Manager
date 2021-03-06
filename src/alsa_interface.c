/*
 * Simple sound playback using ALSA API and libasound.
 *
 * Compile:
 * $ cc -o play sound_playback.c -lasound
 *
 * Usage:
 * $ ./play <sample_rate> <channels> <seconds> < <file>
 *
 * Examples:
 * $ ./play 44100 2 5 < /dev/urandom
 * $ ./play 22050 1 8 < /path/to/file.wav
 *
 * Copyright (C) 2009 Alessandro Ghedini <alessandro@ghedini.me>
 * --------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * Alessandro Ghedini wrote this file. As long as you retain this
 * notice you can do whatever you want with this stuff. If we
 * meet some day, and you think this stuff is worth it, you can
 * buy me a beer in return.
 * --------------------------------------------------------------
 */

#include <alsa/asoundlib.h>
#include <stdio.h>

#define PCM_DEVICE "default"

unsigned int rate = 44100;
unsigned int channels = 2;
snd_pcm_t *pcm_handle;

int alsa_init(void) {
  unsigned int pcm, tmp, dir;
  snd_pcm_hw_params_t *params;
  int buff_size, loops;

  /* Open the PCM device in playback mode */
  pcm = snd_pcm_open(&pcm_handle, PCM_DEVICE, SND_PCM_STREAM_PLAYBACK, 0);
  if (pcm < 0) {
    printf("ERROR: Can't open \"%s\" PCM device. %s\n",
	   PCM_DEVICE, snd_strerror(pcm));
  }

  /* Allocate parameters object and fill it with default values*/
  snd_pcm_hw_params_alloca(&params);

  snd_pcm_hw_params_any(pcm_handle, params);

  /* Set parameters */
  pcm = snd_pcm_hw_params_set_access(pcm_handle, params,
					 SND_PCM_ACCESS_RW_INTERLEAVED);
  if (pcm < 0) {
    printf("ERROR: Can't set interleaved mode. %s\n", snd_strerror(pcm));
    return pcm;
  }

  pcm = snd_pcm_hw_params_set_format(pcm_handle, params, SND_PCM_FORMAT_S16_LE);
  if (pcm < 0) {
    printf("ERROR: Can't set format. %s\n", snd_strerror(pcm));
    return pcm;
  }

  pcm = snd_pcm_hw_params_set_channels(pcm_handle, params, channels);
  if (pcm < 0) {
    printf("ERROR: Can't set channels number. %s\n", snd_strerror(pcm));
    return pcm;
  }

  pcm = snd_pcm_hw_params_set_rate_near(pcm_handle, params, &rate, 0);
  if (pcm < 0) {
    printf("ERROR: Can't set rate. %s\n", snd_strerror(pcm));
    return pcm;
  }

  /* Write parameters */
  pcm = snd_pcm_hw_params(pcm_handle, params);
  if (pcm < 0) {
    printf("ERROR: Can't set harware parameters. %s\n", snd_strerror(pcm));
    return pcm;
  }

  pcm = snd_pcm_prepare(pcm_handle);
  if (pcm < 0) {
    printf("ERROR: Can't prepare pcm. %s\n", snd_strerror(pcm));
    return pcm;
  }

  /* Resume information */
  printf("PCM name: '%s'\n", snd_pcm_name(pcm_handle));

  printf("PCM state: %s\n", snd_pcm_state_name(snd_pcm_state(pcm_handle)));

  snd_pcm_hw_params_get_channels(params, &tmp);
  printf("channels: %i ", tmp);

  if (tmp == 1) {
    printf("(mono)\n");
  } else if (tmp == 2) {
    printf("(stereo)\n");
  }

  snd_pcm_hw_params_get_rate(params, &tmp, 0);
  printf("rate: %d bps\n", tmp);

   /* Allocate buffer to hold single period */
/*   snd_pcm_hw_params_get_period_size(params, &frames, 0); */
/*    */
/*   buff_size = frames * channels * 2 / * 2 -> sample size * /; */
/*   buff = (char *) malloc(buff_size); */
/*   snd_pcm_hw_params_get_period_time(params, &tmp, NULL); */

  return 0;
}

static int xrun_recovery(snd_pcm_t *handle, int err)
{
  printf("stream recovery\n");
  if (err == -EPIPE) {    /* under-run */
    err = snd_pcm_prepare(handle);
    if (err < 0)
      printf("Can't recovery from underrun, prepare failed: %s\n", snd_strerror(err));
    return 0;
  } else if (err == -ESTRPIPE) {
    while ((err = snd_pcm_resume(handle)) == -EAGAIN)
      sleep(1);       /* wait until the suspend flag is released */
    if (err < 0) {
      err = snd_pcm_prepare(handle);
      if (err < 0)
	printf("Can't recovery from suspend, prepare failed: %s\n", snd_strerror(err));
    }
    return 0;
  }
  return err;
}

int alsa_send(signed short *buff, unsigned int frames) {

  int err;

  while (frames > 0) {
    err = snd_pcm_writei(pcm_handle, buff, frames);
    if (err == -EAGAIN) {
      continue;
    }
    if (err < 0) {
      if (xrun_recovery(pcm_handle, err) < 0) {
	printf("Write error: %s\n", snd_strerror(err));
	return -1;
      }
      break;  /* skip one period */
    }
    buff += err * channels;
    frames -= err;
  }
  return 0;
/*   unsigned int pcm; */
/*   pcm = snd_pcm_writei(pcm_handle, buff, size); */
/*   if (pcm == -EPIPE) { */
/*     printf("XRUN.\n"); */
/*     snd_pcm_prepare(pcm_handle); */
/*   } else if (pcm < 0) { */
/*     printf("ERROR. Can't write to PCM device. %s\n", snd_strerror(pcm)); */
/*     return 1; */
/*   } */
/*    */
  return 0;
}

int alsa_close(void) {
  snd_pcm_drain(pcm_handle);
  snd_pcm_close(pcm_handle);
  return 0;
}
