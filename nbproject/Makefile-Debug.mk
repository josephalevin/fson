#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=GNU-Linux-x86
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/src/fson_path_m.o \
	${OBJECTDIR}/src/fson_example.o \
	${OBJECTDIR}/src/fson.o \
	${OBJECTDIR}/src/fson_string_m.o \
	${OBJECTDIR}/src/fson_value_m.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=-J build

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fson

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fson: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fson ${OBJECTFILES} ${LDLIBSOPTIONS} 

${OBJECTDIR}/src/fson_path_m.o: src/fson_path_m.f95 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/fson_path_m.o src/fson_path_m.f95

${OBJECTDIR}/src/fson_example.o: src/fson_example.f95 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/fson_example.o src/fson_example.f95

${OBJECTDIR}/src/fson.o: src/fson.f95 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/fson.o src/fson.f95

${OBJECTDIR}/src/fson_string_m.o: src/fson_string_m.f95 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/fson_string_m.o src/fson_string_m.f95

${OBJECTDIR}/src/fson_value_m.o: src/fson_value_m.f95 
	${MKDIR} -p ${OBJECTDIR}/src
	$(COMPILE.f) -g -o ${OBJECTDIR}/src/fson_value_m.o src/fson_value_m.f95

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fson
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
