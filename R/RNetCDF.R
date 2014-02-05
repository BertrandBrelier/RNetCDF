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


