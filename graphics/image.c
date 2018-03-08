/* 10/88 jc */
/* $Id: image.c,v 1.6 2002-07-11 23:15:37 burghart Exp $ */
/*
 * Routines related to image save/restore functions. 
 */
# include "param.h"
# include "graphics.h"
# include "workstation.h"
# include "device.h"
# include "image.h"


/*
 * This structure describes an open image file.
 */
struct ifile
{
	int	if_bio_lun;		/* Our I/O unit			*/
	struct if_hdr if_header;	/* The file header		*/
	struct if_toc *if_contents;	/* The table of contents	*/
};

/*
 * Forwards
 */
void Gi_inc_toc(), Gi_offset();





int
Gi_create (file, tag)
char *file;
image_file *tag;
/*
 * Create an image file.
 */
{
	struct ifile *ifl;
	char fname[200];
	int len, zero = 0, one = 1;
/*
 * Get a file structure.
 */
 	ifl = (struct ifile *) getvm (sizeof (struct ifile));
/*
 * Fix up the file name, and try to open it.
 */
	fixdir_t ("IMAGE_DIR", ".", file, fname, ".gif");
	len = strlen (fname);
	if ((ifl->if_bio_lun = bio_create (fname, &len, &zero, &zero)) == 0)
	{
		relvm (ifl);
		return (GE_BAD_FILE);
	}
/*
 * Fill in the header.
 */
	ifl->if_header.ih_version = IF_VERSION;
	ifl->if_header.ih_nimage = 0;
	ifl->if_header.ih_toc = 2;
	ifl->if_header.ih_toc_size = INITTOC;
	ifl->if_header.ih_ffb = 
		(INITTOC*sizeof (struct if_toc) + BLOCKSIZE - 1)/BLOCKSIZE + 2;
/*
 * Allocate in-core TOC space, and write out the file.
 */
 	ifl->if_contents = (struct if_toc *)
		getvm (INITTOC*sizeof (struct if_toc));
	len = sizeof (struct if_hdr);
	bio_write (&ifl->if_bio_lun, &one, &ifl->if_header, &len, &one);
	len = INITTOC*sizeof (struct if_toc);
	bio_write (&ifl->if_bio_lun, &ifl->if_header.ih_toc, ifl->if_contents,
		&len, &one);
/*
 * Return the image file tag.
 */
	*tag = (image_file) ifl;
 	return (GE_OK);
}





int
Gi_open (file, tag, nimage)
char *file;
image_file *tag;
int *nimage;
/*
 * Open an existing image file.
 */
{
	struct ifile *ifl;
	char fname[200];
	int len, zero = 0, one = 1, junk;
/*
 * Get a file structure.
 */
 	ifl = (struct ifile *) getvm (sizeof (struct ifile));
/*
 * Fix up the file name, and try to open it.
 */
	fixdir_t ("IMAGE_DIR", ".", file, fname, ".gif");
	len = strlen (fname);
	if ((ifl->if_bio_lun = bio_open (fname, &len, &junk)) == 0)
	{
		relvm (ifl);
		return (GE_BAD_FILE);
	}
/*
 * Pull in the header.
 */
 	len = sizeof (struct if_hdr);
 	bio_read (&ifl->if_bio_lun, &one, &ifl->if_header, &len, &one);
	if (ifl->if_header.ih_version != IF_VERSION)
	{
		bio_close (&ifl->if_bio_lun);
		relvm (ifl);
		return (GE_NOT_IFILE);
	}
/*
 * Get the table of contents.
 */
 	len = ifl->if_header.ih_toc_size*sizeof (struct if_toc);
	ifl->if_contents = (struct if_toc *) getvm (len);
	bio_read (&ifl->if_bio_lun, &ifl->if_header.ih_toc, ifl->if_contents,
		&len, &one);
/*
 * Return the info.
 */
 	*tag = (image_file) ifl;
	*nimage = ifl->if_header.ih_nimage;
	return (GE_OK);
}




int
Gi_close (tag)
image_file tag;
/*
 * Close this image file.
 */
{
	struct ifile *ifl = (struct ifile *) tag;

	bio_close (&ifl->if_bio_lun);
	relvm (ifl->if_contents);
	relvm (ifl);
	return (GE_OK);
}





int
Gi_nimage (tag)
image_file tag;
/*
 * Return the number of images stored in this file.
 */
{
	struct ifile *ifl = (struct ifile *) tag;

	return (ifl->if_header.ih_nimage);
}




int
Gi_info (tag, image, description, x, y, top)
image_file tag;
int image, *x, *y, *top;
char *description;
/*
 * Return some information on an image in a file.
 * Entry:
 *	TAG	is a tag for an open image file.
 *	IMAGE	is the number of the image of interest.
 * Exit:
 *	DESCRIPTION is the textual description of the image.
 *	X, Y	are the X and Y size of the image.
 *	TOP	is TRUE iff this is a top-origin image.
 */
{
	struct ifile *ifl = (struct ifile *) tag;
/*
 * Make sure this is a real image.
 */
 	if (image < 0 || image >= ifl->if_header.ih_nimage)
		return (GE_BAD_IMAGE);
/*
 * OK, get the stuff out of the TOC.
 */
 	strcpy (description, ifl->if_contents[image].it_desc);
	*x = ifl->if_contents[image].it_x;
	*y = ifl->if_contents[image].it_y;
	*top = ifl->if_contents[image].it_flags & IFF_TOP;
	return (GE_OK);
}






int
Gi_save (wstn, tag, description)
ws wstn;
image_file tag;
char *description;
/*
 * Save an image to the file.
 * Entry:
 *	WSTN	is the workstation from which the image is to be saved.
 *	TAG	is an open image file.
 *	DESCRIPTION is the description to be saved with the image.
 * Exit:
 *	The image has been saved.
 */
{
	struct ifile *ifl = (struct ifile *) tag;
	struct workstation *sta = (struct workstation *) wstn;
	struct device *dev = sta->ws_dev;
	struct if_toc *toc;
	char *data;
	int len, one = 1;
/*
 * If we can not read the screen, this can not be done.
 */
	if (! dev->gd_readscreen)
		return (GE_DEVICE_UNABLE);
/*
 * Increment the image count, and grab more TOC space if necessary.
 */
	if (++(ifl->if_header.ih_nimage) >= ifl->if_header.ih_toc_size)
		Gi_inc_toc (ifl);
/*
 * Fill in the new contents entry.
 */
	toc = ifl->if_contents + (ifl->if_header.ih_nimage - 1);
	strncpy (toc->it_desc, description, DESCSIZE-1);
	toc->it_desc[DESCSIZE-1] = '\0';
	toc->it_x = dev->gd_xres;
	toc->it_y = dev->gd_yres;
/*
 * Allocate some file space.
 */
	toc->it_offset = ifl->if_header.ih_ffb;
	ifl->if_header.ih_ffb +=
		(toc->it_x * toc->it_y + BLOCKSIZE - 1)/BLOCKSIZE;
/*
 * Write out the header and toc.
 */
 	len = sizeof (struct if_hdr);
	bio_write (&ifl->if_bio_lun, &one, &ifl->if_header, &len, &one);
	len = ifl->if_header.ih_toc_size * sizeof (struct if_toc);
	bio_write (&ifl->if_bio_lun, &ifl->if_header.ih_toc, ifl->if_contents,
		&len, &one);
/*
 * Now go for it.  This huge I/O operation will have to change at some point.
 */
	len = toc->it_x * toc->it_y;
	data = getvm (len);
	(*dev->gd_readscreen)(sta->ws_tag, 0, 0, dev->gd_xres, dev->gd_yres,
		data);
	if (dev->gd_coff)
		Gi_offset (data, len, -(*dev->gd_coff) (sta->ws_tag));
	bio_write (&ifl->if_bio_lun, &toc->it_offset, data, &len, &one);
	relvm (data);
/*
 * All done.
 */
 	return (GE_OK);
}



void
Gi_inc_toc (ifl)
struct ifile *ifl;
/*
 * Increase the size of the TOC in this file.
 */
{
	struct if_toc *newtoc;
	int len;
/*
 * Figure out what our new TOC size will be, and allocate the memory.
 */
 	len = 2*ifl->if_header.ih_toc_size*sizeof (struct if_toc);
	ifl->if_header.ih_toc_size *= 2;
	newtoc = (struct if_toc *) getvm (len);
	memcpy (newtoc, ifl->if_contents, len/2);
	relvm (ifl->if_contents);
	ifl->if_contents = newtoc;
/*
 * Find some disk space.  For now, we just throw away the old space.
 */
 	ifl->if_header.ih_toc = ifl->if_header.ih_ffb;
	ifl->if_header.ih_ffb += (len + BLOCKSIZE - 1)/BLOCKSIZE;
}




int
Gi_restore (wstn, tag, image)
ws wstn;
image_file tag;
int image;
/*
 * Restore an image to the screen.
 * Entry:
 *	WSTN	is a workstation tag.
 *	TAG	is an open image file tag.
 *	IMAGE	is the number of the image to restore.
 * Exit:
 *	The image has been restored.
 */
{
	struct ifile *ifl = (struct ifile *) tag;
	struct workstation *sta = (struct workstation *) wstn;
	struct device *dev = sta->ws_dev;
	struct if_toc *toc;
	char *data;
	int len, one = 1;
/*
 * No can do without a pixel device.
 */
 	if ((dev->gd_flags & GDF_PIXEL) == 0)
		return (GE_DEVICE_UNABLE);
	if (image < 0 || image >= ifl->if_header.ih_nimage)
		return (GE_BAD_IMAGE);
/*
 * For the time being, we can't deal with images bigger than the screen.
 */
	toc = ifl->if_contents + image;
	if (toc->it_x > dev->gd_xres || toc->it_y > dev->gd_yres)
		return (GE_DEVICE_UNABLE);
/*
 * Allocate memory for the image, and read it in.
 */
	len = toc->it_x * toc->it_y;
	data = getvm (len);
	bio_read (&ifl->if_bio_lun, &toc->it_offset, data, &len, &one);
	if (dev->gd_coff)
		Gi_offset (data, len, (*dev->gd_coff) (sta->ws_tag));
/*
 * Ship it out to the device.
 */
 	(*dev->gd_pixel) (sta->ws_tag, 0, 0, toc->it_x, toc->it_y, data,
		GP_BYTE, GPO_RASTOR);
	(*dev->gd_flush) (sta->ws_tag);
	relvm (data);
	return (GE_OK);
}




void
Gi_offset (data, len, offset)
register char *data;
register int len, offset;
/*
 * Apply this offset to the data.
 */
{
	printf ("Offsetting, len %d offset %d\n", len,  offset);
	if (! offset)
		return;
	for (; len > 0; len--)
		*data++ += offset;
}
