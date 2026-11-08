// Loading the firmware file into HyperRAM. Touches nothing but FatFs and the image's
// memory, so host/test_boot.c runs it on the Mac, with FW_MEM pointing into its own buffer.
#include "load.h"
#include "crc32.h"
#include "ff.h"
#include "m2m_rv.h"

#ifndef FW_MEM
#define FW_MEM(a) ((void *)(uintptr_t)(a))
#endif

static FATFS fs;
static FIL   file;

const char fw_err_mount[] = "Can't read the SD card";

const char *fw_load(const char *path, fw_block_t *hdr)
{
   fw_header_t *h = &hdr->h;
   UINT n;

   h->magic = 0;
   if (f_mount(&fs, "", 1) != FR_OK)
      return fw_err_mount;
   if (f_open(&file, path, FA_READ) != FR_OK)
      return "The firmware file is missing";
   if (f_read(&file, hdr->block, FW_HEADER_SIZE, &n) != FR_OK || n != FW_HEADER_SIZE ||
       h->magic != FW_MAGIC || h->header_ver != FW_HEADER_VER)
      return "The firmware file is no firmware";
   if (h->hw_ver != M2M_RV_HW_VERSION)
      return "The firmware doesn't fit this core";
   if (h->load_addr < M2M_FW_BASE || h->load_addr >= M2M_FW_END || (h->load_addr & 3) ||
       h->size == 0 || h->size > M2M_FW_END - h->load_addr || h->entry - h->load_addr >= h->size)
      return "The firmware's header is broken";

   // Whole blocks go straight from the card to HyperRAM
   if (f_read(&file, FW_MEM(h->load_addr), h->size, &n) != FR_OK)
      return "Can't read the firmware";
   if (n != h->size)
      return "The firmware file is cut short";

   // Read it back from HyperRAM, a word at a time
   uint32_t crc = 0;
   for (uint32_t i = 0; i < h->size; i += 4) {
      uint32_t w = *(volatile uint32_t *)FW_MEM(h->load_addr + i);
      crc = crc32_update(crc, &w, h->size - i < 4 ? h->size - i : 4);
   }
   if (crc != h->crc32)
      return "The firmware's checksum is wrong";
   return 0;
}
