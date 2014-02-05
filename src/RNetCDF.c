/*=============================================================================*\
 *									       *
 *  Name:       RNetCDF.c						       *
 *									       *
 *  Version:    							       *
 *									       *
 *  Purpose:    NetCDF interface for R.					       *
 *									       *
 *  Author:     Bertrand Brelier (bertrand.brelier@gmail.com)
                Pavel Michna (michna@giub.unibe.ch)			       *
 *              Milton Woods (m.woods@bom.gov.au)                              *
 *									       *
 *  Copyright:  (C) 2004-2014 Pavel Michna                                     *
 *									       *
 *=============================================================================*
 *									       *
 *  This program is free software; you can redistribute it and/or modify       *
 *  it under the terms of the GNU General Public License as published by       *
 *  the Free Software Foundation; either version 2 of the License, or	       *
 *  (at your option) any later version. 				       *
 *									       *
 *  This program is distributed in the hope that it will be useful,	       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of	       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	       *
 *  GNU General Public License for more details.			       *
 *									       *
 *  You should have received a copy of the GNU General Public License	       *
 *  along with this program; if not, write to the Free Software 	       *
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *									       *


/*=============================================================================*\
 *  Includes								       *
\*=============================================================================*/

#include <stdio.h>
#include <string.h>

#include <netcdf.h>
#include <udunits.h>

#include <R.h>
#include <Rinternals.h>



/*=============================================================================*\
 *  NetCDF library functions						       *
\*=============================================================================*/

/*-----------------------------------------------------------------------------*\
 *  R_nc_create() Modified for NetCDF4                                                              *
\*-----------------------------------------------------------------------------*/


SEXP R_nc_create (SEXP filename, SEXP type)
{
  int  cmode;
  int ncid;
  int status;
  SEXP retlist, retlistnames;

  /*-- Create output object and initialize return values --------------------*/
  PROTECT(retlist = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
  SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

  PROTECT(retlistnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  SET_STRING_ELT(retlistnames, 2, mkChar("ncid"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  ncid   = -1;
  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;


  if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_NOCLOBBER"  ) == 0)
    cmode = NC_NOCLOBBER;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_NOCLOBBER|NC_64BIT_OFFSET"  ) == 0)
    cmode = NC_NOCLOBBER|NC_64BIT_OFFSET;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_NOCLOBBER|NC_NETCDF4"  ) == 0)
    cmode = NC_NOCLOBBER|NC_NETCDF4;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_NOCLOBBER|NC_NETCDF4|NC_CLASSIC_MODEL"  ) == 0)
    cmode = NC_NOCLOBBER|NC_NETCDF4|NC_CLASSIC_MODEL;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_SHARE"  ) == 0)
    cmode = NC_SHARE;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_64BIT_OFFSET"  ) == 0)
    cmode = NC_64BIT_OFFSET;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_NETCDF4"  ) == 0)
    cmode = NC_NETCDF4;
  else if     (strcmp(CHAR(STRING_ELT(type, 0)), "NC_CLASSIC_MODEL"  ) == 0)
    cmode = NC_CLASSIC_MODEL;
  else {
    SET_VECTOR_ELT (retlist, 1, mkString("Unknown NC_File type"));
    REAL(VECTOR_ELT(retlist, 0))[0] = -1;
    UNPROTECT(2);
    return(retlist);
  }

  status = nc_create(R_ExpandFileName(CHAR(STRING_ELT(filename, 0))),
		     cmode, &ncid);

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)ncid;
  UNPROTECT(2);
  return(retlist);
}

/*-----------------------------------------------------------------------------*\                                                                                                     
*  R_ut_init()                                                                *                                                                                                       
\*-----------------------------------------------------------------------------*/

SEXP R_ut_init (SEXP path)
{
  int   status;
  char  strerror[64];
  SEXP  retlist, retlistnames;

  /*-- Avoid "overriding default" messages from UDUNITS-2 (1/2) -------------*/
    #ifdef UT_UNITS2_H_INCLUDED
  ut_system* unitSystem;

  ut_set_error_message_handler(ut_ignore);
  unitSystem = ut_read_xml(NULL);
    #endif

  /*-- Create output object and initialize return values --------------------*/
  PROTECT(retlist = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));

  PROTECT(retlistnames = allocVector(STRSXP, 2));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));

  /*-- Initialize udunits library -------------------------------------------*/
  status = utInit(R_ExpandFileName(CHAR(STRING_ELT(path, 0))));
  if(status != 0) {
    R_ut_strerror(status, strerror);
    SET_VECTOR_ELT(retlist, 1, mkString(strerror));
  }

  /*-- Avoid "overriding default" messages from UDUNITS-2 (2/2) -------------*/
    #ifdef UT_UNITS2_H_INCLUDED
  ut_set_error_message_handler(ut_write_to_stderr);
    #endif

  /*-- Returning the list ---------------------------------------------------*/
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  UNPROTECT(2);
  return(retlist);
}
