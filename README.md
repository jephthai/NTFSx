NTFSx README File
-----------------

Author: Joshua Stone <yakovdk@gmail.com>
Date: 2012-06-03 13:55:40 


Table of Contents
-----------------

1. Introduction 
2. Method of operation 
3. Known Limitations 
4. Implementation 


1 Introduction 
--------------

NTFSx is a tool that extracts files from a running system's NTFS
filesystem.  This is accomplished in a non-intrusive fashion.  

- Only Administrator-level permissions are required
- No changes are made to the system
- There is no dependency on particular service configurations

This is particularly handy when you need to extract a copy of system
files such as NTDS.dit (the storage location for account details, 
including password hashes, in a modern Active Directory domain). 
There may also be other times when this may be useful, so you can
use NTFSx to extract any file from the filesystem.

2 Method of operation 
---------------------

NTFSx works at the hard drive level.  It parses the NTFS data
structures on the disk and identifies the block-offsets at which the
operating system stores the the desired file(s).

Once identifying these locations, NTFSx extracts these one block at a
time into new files stored in the current directory.  Since the
physical device file is opened with READ-ONLY rights, this is a very
safe and non-intrusive operation.  It turns out that Microsoft chose
to permit administrator users to access physical devices (e.g., the
"\\.\C:" device, which is the C: drive), thus not requiring SYSTEM
access.

Due to the existence of many system administration tools that access
raw disk volumes, Microsoft considers this not to be a significant
security issue.  Nevertheless, it is useful in penetration tests,
security audits, etc.

3 Known Limitations 
-------------------

NTFSx is a relatively simple implementation, and thus incurs certain
limitations.  These have been deemed acceptable for a publicly
released tool.  It is possible that future releases of NTFSx may
eliminate some of these limitations:

- The %SYSTEMROOT% is assumed to be the C: drive

- Some race conditions may exist, such as if the kernel is in the
  process of writing to SAM or SYSTEM.

  - My recommendation would be to just run it again if you get a
    corrupt file

4 Example Usage
---------------

You will need to copy the EXE file up to the system from which you
want to extract files.  Running NTFSx without arguments will give you
a usage statement that may be more current than this README file.

To extract the NTDS password databases from a running system, the
following command should generally work.  It may take several minutes,
depending on the size of the master file table (MFT).

    C:\>ntfsx ntds

You can also extract a file by specifying the path.  The second
argument specifies the filename to write with the contents:

    C:\>ntfsx Windows\system32\config\SYSTEM SYSTEM.OUT

4 Implementation 
----------------

NTFSx is written exclusively in the Haskell programming language,
using the Glasgow Haskell Compiler suite (GHC).  It features a native
implementation of NTFS (at least for the purposes of parsing and
reading it), also written in Haskell.  The executable should be quite
portable, as it does not rely on any runtime libraries or environment.
