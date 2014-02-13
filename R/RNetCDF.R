#===============================================================================#
#										#
#  Name:       RNetCDF.R							#
#										#
#  Version:        								#
#										#
#  Purpose:    NetCDF4 interface for R.						#
#										#
#  Author:     Bertrand Brelier (bertrand.brelier@gmail.com)			#
#  	       Pavel Michna (michna@giub.unibe.ch)				#
#              Milton Woods (m.woods@bom.gov.au)                                #
#										#
#  Copyright:  (C) 2004-2014 Pavel Michna					#
#										#
#===============================================================================#
#										#
#  This program is free software; you can redistribute it and/or modify 	#
#  it under the terms of the GNU General Public License as published by 	#
#  the Free Software Foundation; either version 2 of the License, or		#
#  (at your option) any later version.						#
#										#
#  This program is distributed in the hope that it will be useful,		#
#  but WITHOUT ANY WARRANTY; without even the implied warranty of		#
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		#
#  GNU General Public License for more details. 				#
#										#
#  You should have received a copy of the GNU General Public License		#
#  along with this program; if not, write to the Free Software			#
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA	#
#										#


#===============================================================================#
#  NetCDF library functions							#
#===============================================================================#

#-------------------------------------------------------------------------------#
#  create.nc()                                                                  #
#-------------------------------------------------------------------------------#

create.nc <- function(filename,type)
{
    nc <- .Call("R_nc_create",
                as.character(filename),
		as.character(type),
                PACKAGE="RNetCDF")
    #-- Return object if no error ----------------------------------------------#
    if(nc$status == 0) {
        ncfile <- nc$ncid
        attr(ncfile, "class") <- "NetCDF"
        return(ncfile)
    } else
        stop(nc$errmsg, call.=FALSE)
}

DataFrame.read <- function(dataframe)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(dataframe) == "data.frame")

    Nrow <- nrow(dataframe)
    Column <- dataframe[[2]]
    nc <- .Call("R_nc_read_DataFrame",
                as.integer(Nrow),
		as.integer(Data[[2]]),
                PACKAGE="RNetCDF")	

    #ncfile <- nc$Nrow
    #ncfile <- nc$Col1
    return(nc)
}

dim.def.nc <- function(ncfile, dimname, dimension)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")

    nc <- .Call("R_nc_def_dim",
		as.integer(ncfile),
		as.character(dimname),
		as.integer(dimension),
		PACKAGE="RNetCDF")
}

compound.def.nc <- function(ncfile, size, name)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")

    nc <- .Call("R_nc_def_compound",
                as.integer(ncfile),
                as.integer(size),
		as.character(name),
                PACKAGE="RNetCDF")
    #-- Return object if no error ----------------------------------------------#
    if(nc$status == 0) {
        nctypeid <- nc$mtypeid
        attr(nctypeid, "class") <- "NC_COMPOUND"
        attr(nctypeid, "size") <- as.integer(size)
        attr(nctypeid, "NVar") <- as.integer(0)
        attr(nctypeid, "OffSet") <- as.integer(0)
        return(nctypeid)
    } else
        stop(nc$errmsg, call.=FALSE)
}

compound.inq.nc <- function(ncfile, typeid)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")
    stopifnot(class(typeid) == "NC_COMPOUND")

    nc <- .Call("R_nc_inq_compound",
          as.integer(ncfile),
          as.integer(typeid),
          PACKAGE="RNetCDF")
}   

var.def.nc <- function(ncfile,typeid,name,spin)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")
    stopifnot(class(typeid) == "NC_COMPOUND")

    nc <- .Call("R_nc_def_var",
          as.integer(ncfile),
          as.integer(typeid),
          as.character(name),
          as.integer(spin),
          PACKAGE="RNetCDF")

    #-- Return object if no error ----------------------------------------------#
    if(nc$status == 0) {
        ncvarid <- nc$varid
	return(ncvarid)
    } else
        stop(nc$errmsg, call.=FALSE)			
}

compound.insert.nc <- function(ncfile, typeid, name, field_typeid, dim=1)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")
    stopifnot(class(typeid) == "NC_COMPOUND")

    nc <- .Call("R_nc_insert_compound",
          as.integer(ncfile),
          as.integer(typeid),
          as.character(name),
          as.integer(attr(typeid, "OffSet")),
          as.character(field_typeid),
	  as.integer(dim),
          PACKAGE="RNetCDF")

    attr(typeid, "NVar") <- as.integer(attr(typeid,'NVar')+1)
    attr(typeid, "VarName") <- append(attr(typeid, "VarName"),as.character(field_typeid))
    attr(typeid, "Dim") <- append(attr(typeid, "Dim"),as.integer(dim))
  if(as.character(field_typeid)=="NC_INT"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+4*dim)
    }

  if(as.character(field_typeid)=="NC_BYTE"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+1*dim)
    }

  if(as.character(field_typeid)=="NC_CHAR"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+1*dim)
    }

  if(as.character(field_typeid)=="NC_SHORT"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+2*dim)
    }

  if(as.character(field_typeid)=="NC_FLOAT"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+4*dim)
    }

  if(as.character(field_typeid)=="NC_DOUBLE"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+8*dim)
    }

  if(as.character(field_typeid)=="NC_UBYTE"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+1*dim)
    }

  if(as.character(field_typeid)=="NC_USHORT"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+2*dim)
    }

  if(as.character(field_typeid)=="NC_UINT"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+4*dim)
    }

  if(as.character(field_typeid)=="NC_INT64"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+8*dim)
    }

  if(as.character(field_typeid)=="NC_UINT64"){
       attr(typeid, "OffSet") <- as.integer(attr(typeid,'OffSet')+8*dim)
    }


    return(typeid)
}

compound.fill.nc <- function(ncfile , nctypeid, varid, TheData)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(ncfile) == "NetCDF")
    stopifnot(class(nctypeid) == "NC_COMPOUND")


    nc <- .Call("R_nc_fill_compound",
                as.integer(ncfile),
                as.integer(nctypeid),
                as.integer(varid),
                as.integer(attr(nctypeid, "OffSet")),
                as.integer(attr(nctypeid,'NVar')),
		as.character(attr(nctypeid,'VarName')),
		as.list(attr(nctypeid,'Dim')),
		as.data.frame(TheData),	
                PACKAGE="RNetCDF")
}

#-------------------------------------------------------------------------------#
#  close.nc()                                                                   #
#-------------------------------------------------------------------------------#

close.nc <- function(con, ...)
{
    #-- Check args -------------------------------------------------------------#
    stopifnot(class(con) == "NetCDF")

    #-- C function call --------------------------------------------------------#
    nc <- .Call("R_nc_close",
                as.integer(con),
			PACKAGE="RNetCDF")

    if(nc$status != 0)
        stop(nc$errmsg, call.=FALSE)
}


#-------------------------------------------------------------------------------#
#  utinit.nc()                                                                  #
#-------------------------------------------------------------------------------#

utinit.nc <- function(path="")
{
    ut <- .Call("R_ut_init",
                as.character(path),
                PACKAGE="RNetCDF")

    if(ut$status != 0)
        stop(ut$errmsg, call.=FALSE)
}


