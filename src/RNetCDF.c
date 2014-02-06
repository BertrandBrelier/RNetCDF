/*=============================================================================*\
 *									       *
 *  Name:       RNetCDF.c						       *
 *									       *
 *  Version:    							       *
 *									       *
 *  Purpose:    NetCDF4 interface for R.					       *
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

#define DIM_LEN 10 //number of records in file                                                                                                                                         
#define SVC_REC "Particle"
#define STARDATE "NRecords"
#define SERVICE_RECORD "Data"


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

SEXP R_nc_close (SEXP ncid)
{
  int  status;
  SEXP retlist, retlistnames;

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

  /*-- Close the file -------------------------------------------------------*/
  status = nc_close(INTEGER(ncid)[0]);
  if(status != NC_NOERR)
    SET_VECTOR_ELT(retlist, 1, mkString(nc_strerror(status)));

  /*-- Returning the list ---------------------------------------------------*/
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  UNPROTECT(2);
  return(retlist);
}

SEXP R_nc_def_dim(SEXP ncid, SEXP dimname, SEXP dimension){
  int status;
  int latid;
  SEXP retlist, retlistnames;

  /*-- Create output object and initialize return values --------------------*/
  PROTECT(retlist = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
  SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

  PROTECT(retlistnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  SET_STRING_ELT(retlistnames, 2, mkChar("latid"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  latid   = -1;
  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)latid;

  size_t len;
  //len=0 means NC_UNLIMITED
  len = INTEGER(dimension)[0];

  status = nc_def_dim(INTEGER(ncid)[0], CHAR(STRING_ELT(dimname, 0)), len, &latid);

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)latid;
  UNPROTECT(2);
  return(retlist);
}
SEXP R_nc_def_compound(SEXP ncid){
  int status;
  int mycid;
  int mtypeid;
  SEXP retlist, retlistnames;

  /*-- Create output object and initialize return values --------------------*/
  PROTECT(retlist = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
  SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 1));

  PROTECT(retlistnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  SET_STRING_ELT(retlistnames, 2, mkChar("mtypeid"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  mtypeid   = -1;
  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)mtypeid;


  size_t mysize = 8;
  mycid=INTEGER(ncid)[0];
  status = nc_def_compound(mycid, mysize, SVC_REC, &mtypeid);

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  REAL(VECTOR_ELT(retlist, 2))[0] = (double)mtypeid;
  UNPROTECT(2);
  return(retlist);

}


SEXP R_nc_make_compound(SEXP ncid, SEXP typeid){

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
  SET_STRING_ELT(retlistnames, 2, mkChar("latid"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);



  int mycid, mtypeid, varid;
  size_t nfields;
  int dimid;
  int ndims, nvars, natts, unlimdimid;
  char name[NC_MAX_NAME + 1];
  size_t size;
  nc_type xtype, field_xtype;
  int dimids[] = {0}, fieldid;
  int field_ndims, field_sizes[NC_MAX_DIMS];
  size_t offset;
  int i;
  struct s1
  {
    int i1;
    int i2;
  };
  struct s1 data[DIM_LEN];
  /* Create some phony data. */
  for (i=0; i<DIM_LEN; i++)
    {
      data[i].i1 = 5;
      data[i].i2 = 10;
    }

  mycid=INTEGER(ncid)[0];
  mtypeid = INTEGER(typeid)[0];

  //size_t mysize = sizeof(struct s1);
  size_t mysize = 8;


  //nc_def_compound(mycid, mysize, SVC_REC, &mtypeid);
  
  nc_inq_compound(mycid, mtypeid, name, &size, &nfields);
  size != mysize || strcmp(name, SVC_REC) || nfields;
  nc_insert_compound(mycid, mtypeid, "Var1",
		     NC_COMPOUND_OFFSET(struct s1, i1), NC_INT);
  nc_insert_compound(mycid, mtypeid, "Var2",
		     NC_COMPOUND_OFFSET(struct s1, i2), NC_INT);
  nc_def_dim(mycid, STARDATE, DIM_LEN, &dimid);
  nc_def_var(mycid, SERVICE_RECORD, mtypeid, 1, dimids, &varid);
  nc_put_var(mycid, varid, data);

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  UNPROTECT(2);
  return(retlist);

  
}


/*=============================================================================*\                                                                                                      
 *  Udunits library functions                                                  *                                                                                                       
 \*=============================================================================*/

/*-----------------------------------------------------------------------------*\                                                                                                      
 *  R_ut_strerror()                                                            *                                                                                                       
 \*-----------------------------------------------------------------------------*/

void R_ut_strerror (int errcode, char* strerror)
{
  if     (errcode == UT_EOF     )
    strcpy(strerror, "end-of-file encountered (udunits)");
  else if(errcode == UT_ENOFILE )
    strcpy(strerror, "no units-file (udunits)");
  else if(errcode == UT_ESYNTAX )
    strcpy(strerror, "syntax error (udunits)");
  else if(errcode == UT_EUNKNOWN)
    strcpy(strerror, "unknown specification (udunits)");
  else if(errcode == UT_EIO     )
    strcpy(strerror, "I/O error (udunits)");
  else if(errcode == UT_EINVALID)
    strcpy(strerror, "invalid unit-structure (udunits)");
  else if(errcode == UT_ENOINIT )
    strcpy(strerror, "package not initialized (udunits)");
  else if(errcode == UT_ECONVERT)
    strcpy(strerror, "two units are not convertable (udunits)");
  else if(errcode == UT_EALLOC  )
    strcpy(strerror, "memory allocation failure (udunits)");
  else if(errcode == UT_ENOROOM )
    strcpy(strerror, "insufficient room supplied (udunits)");
  else if(errcode == UT_ENOTTIME)
    strcpy(strerror, "not a unit of time (udunits)");
  else
    strcpy(strerror, "unknown error (udunits)");
}
SEXP R_ut_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
  int    year, month, day, hour, minute, count, i, status;
  float  second;
  double utvalue;
  char   strerror[64];
  utUnit utunit;
  SEXP   retlist, retlistnames;

  /*-- Create output object and initialize return values --------------------*/
  PROTECT(retlist = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
  SET_VECTOR_ELT(retlist, 2, allocMatrix(REALSXP, INTEGER(unitcount)[0], 6));

  PROTECT(retlistnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  SET_STRING_ELT(retlistnames, 2, mkChar("value"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));

  /*-- Scan unitstring ------------------------------------------------------*/
  status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
  if(status != 0) {
    R_ut_strerror(status, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    UNPROTECT(2);
    return(retlist);
  }
  /*-- Check if unit is time and has origin ---------------------------------*/
  status = utIsTime(&utunit);
  if(status == 0) {
    R_ut_strerror(UT_ENOTTIME, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = UT_ENOTTIME;
    UNPROTECT(2);
    return(retlist);
  }

  status = utHasOrigin(&utunit);
  if(status == 0) {
    R_ut_strerror(UT_EINVALID, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = UT_EINVALID;
    UNPROTECT(2);
    return(retlist);
  }

  /*-- Convert values -------------------------------------------------------*/
  count = (int)INTEGER(unitcount)[0];
  for(i=0; i<count; i++) {
    utvalue = (double)REAL(values)[i];
    status  = utCalendar(utvalue, &utunit, &year, &month, &day,
			 &hour, &minute, &second);

    REAL(VECTOR_ELT(retlist, 2))[i+0*count] = (double)year;
    REAL(VECTOR_ELT(retlist, 2))[i+1*count] = (double)month;
    REAL(VECTOR_ELT(retlist, 2))[i+2*count] = (double)day;
    REAL(VECTOR_ELT(retlist, 2))[i+3*count] = (double)hour;
    REAL(VECTOR_ELT(retlist, 2))[i+4*count] = (double)minute;
    REAL(VECTOR_ELT(retlist, 2))[i+5*count] = (double)second;
  }

  /*-- Returning the list ---------------------------------------------------*/
  if(status != 0) {
    R_ut_strerror(status, strerror);
    SET_VECTOR_ELT(retlist, 1, mkString(strerror));
  }

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
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

/*-----------------------------------------------------------------------------*\                                                                                                      
 *  R_ut_inv_calendar()                                                        *                                                                                                       
 \*-----------------------------------------------------------------------------*/

SEXP R_ut_inv_calendar (SEXP unitstring, SEXP unitcount, SEXP values)
{
  int    year, month, day, hour, minute, count, i, status;
  float  second;
  double utvalue;
  char   strerror[64];
  utUnit utunit;
  SEXP   retlist, retlistnames;

  /*-- Create output object and initialize return values --------------------*/
  count = (int)INTEGER(unitcount)[0];
  count = count/6;

  PROTECT(retlist = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
  SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
  SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, count));

  PROTECT(retlistnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(retlistnames, 0, mkChar("status"));
  SET_STRING_ELT(retlistnames, 1, mkChar("errmsg"));
  SET_STRING_ELT(retlistnames, 2, mkChar("value"));
  setAttrib(retlist, R_NamesSymbol, retlistnames);

  status = -1;
  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  SET_VECTOR_ELT (retlist, 1, mkString(""));

  /*-- Scan unitstring ------------------------------------------------------*/
  status = utScan(CHAR(STRING_ELT(unitstring, 0)), &utunit);
  if(status != 0) {
    R_ut_strerror(status, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = status;
    UNPROTECT(2);
    return(retlist);
  }
  /*-- Check if unit is time and has origin ---------------------------------*/
  status = utIsTime(&utunit);
  if(status == 0) {
    R_ut_strerror(UT_ENOTTIME, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = UT_ENOTTIME;
    UNPROTECT(2);
    return(retlist);
  }

  status = utHasOrigin(&utunit);
  if(status == 0) {
    R_ut_strerror(UT_EINVALID, strerror);
    SET_VECTOR_ELT (retlist, 1, mkString(strerror));
    REAL(VECTOR_ELT(retlist, 0))[0] = UT_EINVALID;
    UNPROTECT(2);
    return(retlist);
  }

  /*-- Convert values -------------------------------------------------------*/
  for(i=0; i<count; i++) {
    year   = (int)REAL(values)[i+0*count];
    month  = (int)REAL(values)[i+1*count];
    day    = (int)REAL(values)[i+2*count];
    hour   = (int)REAL(values)[i+3*count];
    minute = (int)REAL(values)[i+4*count];
    second = (double)REAL(values)[i+5*count];

    status = utInvCalendar(year, month, day, hour, minute, second,
			   &utunit, &utvalue);

    REAL(VECTOR_ELT(retlist, 2))[i] = (double)utvalue;
  }

  /*-- Returning the list ---------------------------------------------------*/
  if(status != 0) {
    R_ut_strerror(status, strerror);
    SET_VECTOR_ELT(retlist, 1, mkString(strerror));
  }

  REAL(VECTOR_ELT(retlist, 0))[0] = (double)status;
  UNPROTECT(2);
  return(retlist);
}

