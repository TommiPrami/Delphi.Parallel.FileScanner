// Filename: DPFSUnit.Parallel.FileScanner.inc

// DPFSUnit.Parallel.FileScanner itself needs neither define: TParallelFileScanner (RTL PPL) uses the RTL alone.
// They switch the optional units - and the demo app's and tests' parts that use them - on and off.

// Comment out to build without OmniThreadLibrary: DPFSUnit.Parallel.FileScanner.OTL (TParallelFileScannerOTL)
// then compiles to an empty unit, and the demo app disables its OmniThreadLibrary buttons.
{$DEFINE USE_OMNI_THREAD_LIBRARY}

// Comment out to build without the Spring4D dependency: DPFSUnit.Parallel.FileScanner.Spring
// (TParallelFileScannerSpring) then compiles to an empty unit, and the demo app disables its
// "Spring container parallel scan" button.
{$DEFINE USE_SPRING4D}
