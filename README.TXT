        README file for the PMOS/2 library (Last updated 1996/09/03)
        ==================================

Welcome to PMOS/2.  PMOS/2 is a set of Modula-2 modules designed
to be used in conjunction with the OS/2 operating system.  It is,
in fact, an OS/2 port of the PMOS library, which is a programmer's
library for writing multitasking software either under DOS or in
embedded systems where there is no operating system.  Because of
the major differences in the underlying systems, PMOS/2 is not
quite the same as PMOS, but I've tried to keep the two consistent
to a reasonably high degree.

Subject to certain conditions which are described below, this
software is free.

There is, at this stage, no proper documentation for PMOS/2.
You might find the PMOS documentation useful; it's in a report called
the "PMOS Reference Manual", which can be found at most places
where PMOS is available.  The most definitive documentation,
however, is in the source files themselves.  In particular, you
should check out the contents of the .DEF files.


DISCLAIMER

Like all free software, this package is supplied "as is", and no
warranties are made or implied as to its fitness for any particular
purpose.  The author does not accept responsibility for any damage,
including consequential damage, caused by the use of this software,
except to the extent to which this disclaimer may be overridden by law.


LICENCE INFORMATION

All of the components of the PMOS/2 package may freely be
used subject to the following conditions.  If you do not accept the
conditions, do not use the package.  Your use of any part of PMOS/2
implies your acceptance of these conditions.

1. Neither PMOS/2 nor any derived product may be sold or distributed
   for a fee, or used to enhance the value of a product for which a
   fee is charged, except by prior agreement.  The author is willing
   to negotiate agreements with individuals or companies for commercial
   use of PMOS/2, but in the absence of such an agreement PMOS/2 is
   for non-profit use only.

   [Note to shareware authors and others involved in low-profit
   ventures: under certain circumstances I am willing to grant a
   fee-free licence for the commercial use of PMOS/2.  As a general
   rule I will charge a fee only for substantial commercial use or
   when you are in competition with a company in which I hold an
   interest; and I will not charge a fee for small-scale use or for
   educational use.  Nevertheless the present licence covers only
   non-profit use of PMOS/2, and any other use must be separately
   negotiated.]

   The present licence permits the use of PMOS/2 for evaluation purposes
   or for the building of prototypes which are not for sale.

2. You may give copies of PMOS/2 to other people, and keep copies in
   software repositories such as anonymous ftp servers, but only if this
   README file is included.

3. You may modify PMOS/2, but only if you agree that the altered
   version continues to be subject to the conditions given here.
   The name of the original author(s), and any copyright notices
   contained in these files, may not be deleted in the course of
   making such alterations.


PREREQUISITES

Hardware: Sufficient to run OS/2.

Operating system: This software has been developed using OS/2 Warp,
    and has also been tested on Warp Connect.  It will probably work
    on earlier or later versions of OS/2, but has not been tested
    with those other versions.

Compiler: The software has been developed using XDS Modula-2 (OS/2
    Native Code version).  Since XDS is generally compatible with
    the Modula-2 standard, the source code should be portable to
    other ISO-compliant compilers with little effort.  Note, however,
    that different compilers could have different ways of providing
    an interface to the OS/2 system functions.  PMOS/2 has quite
    a few operating system dependencies, so you might find yourself
    having to modify code wherever something is imported from OS2.DEF.


INSTALLATION

If you have read this far, then you've probably already completed
the installation.  But, just in case something went wrong, here's
what you should have done:

1. Create a directory, e.g. D:\PMOS2, to hold all of the PMOS/2
   files.  (You could use an existing directory, but I advise against
   it; it gets confusing if two or more software packages live in the
   same directory.)

2. Unzip the distribution file into this directory.  If you're using
   InfoZip, appropriate subdirectories are created automatically.
   If you're using PKUnzip, make sure to use the -d option to
   ensure that the subdirectory structure is preserved.


DE-INSTALLATION

If you don't want to keep PMOS/2, simply delete the directory you
created during installation, and all of its contents.
PMOS/2 does not alter your INI files or your CONFIG.SYS, it
doesn't tamper with your PATH, and it does not scatter its files
through your system directories.

(Now, if only we could talk all software providers into following
those rules.)


WHAT'S IN THE DIRECTORIES

After you have installed PMOS, you will have the following directories.

      PMOS2
          Files needed for installation.

      PMOS2\DEF
          Modula-2 definition modules.

      PMOS2\DOC
          Some documentation files.  You should read through these
          at some stage.

      PMOS2\SRC
          The main directory for PMOS/2 source files.  This is where
          you will find almost all of the PMOS/2 source code.

      PMOS2\SRC\ANSI
          Modules that use ANSI.SYS for text-mode screen output.  For now, this
          is the only kind of screen output we support.  (But there are plans to add
          better screen output modules later.)

      PMOS2\TESTS
          Project files for making some test programs.

      PMOS2\TESTS\SRC
          Source code for the test programs.

      PMOS2\UTIL
          Project files for making some utility programs.

      PMOS2\UTIL\DEF and PMOS2\UTIL\SRC
          Source code for the utility programs.

Depending on which version you picked up, you might find some other
subdirectories: object modules, new modules which are not yet working, etc.

[NOTE: The current version of PMOS/2 is a very early release.
Most of PMOS has not yet been ported to PMOS/2.  You'll have to
wait until after a few more releases to get a more substantial
software collection.]

It is strongly recommended that you not put any of your own files
into any of these directories.  Mixing up your own work with the PMOS/2
files is potentially very confusing.  The preferred method is to make
a new top-level directory for your own work, and to use the PMOS2
directory and its subdirectories exclusively for the PMOS/2 components.

If you have both PMOS and PMOS/2, don't put them in the same directory.
That would certainly create conflicts.


CONTACT INFORMATION

The author of PMOS/2 is

            Peter Moylan
            Department of Electrical and Computer Engineering
            The University of Newcastle, NSW 2308
            Australia.

            Phone: +61 49 21 6023      (time zone GMT+10)
            Fax:   +61 49 21 6993
            e-mail:   peter@ee.newcastle.edu.au
            WWW:  http://www.ee.newcastle.edu.au/users/staff/peter/Moylan.html

The preferred method of contacting me is via e-mail; this will probably
bring a faster response than with paper mail.
