/*
** DUG_IDE : Provides IDE disk info on both drives in the system
**           Version 1.0
**
** Copyright (c) 12 Jan 1993 by Doug Merrett - All rights reserved
**
** Postal Address: PO Box 432
**                 Stones Corner  Q  4120
**                 Australia
**
** Email Address:  dcm@mincom.oz.au
**
** You may distribute freely, but no modifications without mailing me a copy.
** No profit can be made for distribution.  Blah Blah Blah etc.
*/

/* Used by JPI for clrscr */
#define _JPI_WIN_


#include <stdlib.h>
#include <dos.h>
#include <stdio.h>
#include <conio.h>
#include <bios.h>


char *getascii (unsigned int in_data [], int off_start, int off_end);

void main (void)
{
  unsigned int dd [256]; /* DiskData */
  unsigned int dd_off;   /* DiskData offset */
  unsigned int loop;     /* Loop variable */
  int num_drv;           /* Number of Hard disks */

  union REGS registers;  /* Used for Interrupt for BIOS data */


  unsigned int bios_cyl [2], bios_head [2], bios_sec [2];  /* Cylinders, Heads, Sectors */


  clrscr ();

  fprintf (stderr, "** DUG_IDE : Provides IDE disk info on both drives in the system\n");
  fprintf (stderr, "** Version 1.0\n");
  fprintf (stderr, "**\n");
  fprintf (stderr, "** Copyright (c) 12 Jan 1993 by Doug Merrett - All rights reserved\n");
  fprintf (stderr, "**\n");
  fprintf (stderr, "** Postal Address: PO Box 432\n");
  fprintf (stderr, "**                 Stones Corner  Q  4120\n");
  fprintf (stderr, "**                 Australia\n");
  fprintf (stderr, "**\n");
  fprintf (stderr, "** Email Address:  dcm@mincom.oz.au\n");
  fprintf (stderr, "**\n");
  fprintf (stderr, "** You may distribute freely, but no modifications without mailing me a copy.\n");
  fprintf (stderr, "** No profit can be made for distribution.  Blah Blah Blah etc.\n\n");
  fprintf (stderr, "Press any key to accept above limitations");
  getch ();

  /* How many disk drives & parameters */
  num_drv = peekb (0x40, 0x75);  /* BIOS Data area, Number of Hard disks */
                                 /* Byte at Segment 40H Offset 75H */

  for (loop = 0; loop < num_drv; loop++)  /* Loop through drives */
    {
      /* Get IDE Drive info */

      while (inp (0x1F7) != 0x50)  /* Wait for controller not busy */
        ;

      outp (0x1F6, (loop == 0 ? 0xA0 : 0xB0)); /* Get first/second drive */

      outp (0x1F7, 0xEC);          /* Get drive info data */

      while (inp (0x1F7) != 0x58)  /* Wait for data ready */
        ;

      for (dd_off = 0; dd_off != 256; dd_off++) /* Read "sector" */
        dd [dd_off] = inpw (0x1F0);

      /* Get BIOS drive info */
      registers.h.ah = 0x8;            /* Get drive info */
      registers.h.dl = 0x80 + loop;    /* Drive is 80H for Disk 0, 81H for Disk 1 */
      int86 (0x13, &registers, &registers);
      if (! registers.x.cflag)   /* All OK if carry not set */
        {
          bios_head [loop] = registers.h.dh + 1; /* Heads are from 0 */
          bios_sec [loop] = registers.h.cl & 0x3F; /* sec is bits 5 - 0 */
          bios_cyl [loop] = ((registers.h.cl & 0xC0) << 2) + registers.h.ch + 2; /* +1 because starts from 0 and +1 for FDISK leaving one out */
        }

      clrscr ();

      fprintf (stdout, "DRIVE %d:\n", loop);
      fprintf (stdout, "Model Number______________________: %s\n", getascii (dd, 27, 46));
      fprintf (stdout, "Serial Number_____________________: %s\n", getascii (dd, 10, 19));
      fprintf (stdout, "Controller Revision Number________: %s\n\n", getascii (dd, 23, 26));
      fprintf (stdout, "Able to do Double Word Transfer___: %6s\n", (dd [48] == 0 ? "No" : "Yes"));
      fprintf (stdout, "Controller type___________________:   %04X\n", dd [20]);
      fprintf (stdout, "Controller buffer size (bytes)____: %6u\n", dd [21] * 512);
      fprintf (stdout, "Number of ECC bytes transferred___: %6u\n", dd [22]);
      fprintf (stdout, "Number of sectors per interrupt___: %6u\n\n", dd [47]);
      fprintf (stdout, "Hard Disk Reports\n");
      fprintf (stdout, "Number of Cylinders (Fixed)_______: %6u\n", dd [1]);
      fprintf (stdout, "Number of Heads___________________: %6u\n", dd [3]);
      fprintf (stdout, "Number of Sectors per Track_______: %6u\n\n", dd [6]);
      fprintf (stdout, "BIOS Reports\n");
      fprintf (stdout, "Number of Cylinders_______________: %6u\n", bios_cyl [loop]);
      fprintf (stdout, "Number of Heads___________________: %6u\n", bios_head [loop]);
      fprintf (stdout, "Number of Sectors per Track_______: %6u\n\n", bios_sec [loop]);

      if (! loop)
        {
          fprintf (stdout, "Press a key");
          getch ();
        }
   }
}

char *getascii (unsigned int in_data [], int off_start, int off_end)
{
  static char ret_val [255];
  int loop, loop1;

  for (loop = off_start, loop1 = 0; loop <= off_end; loop++)
    {
      ret_val [loop1++] = (char) (in_data [loop] / 256);  /* Get High byte */
      ret_val [loop1++] = (char) (in_data [loop] % 256);  /* Get Low byte */
    }
  ret_val [loop1] = '\0';  /* Make sure it ends in a NULL character */
  return (ret_val);
}
