{$IFNDEF FPC}
{$IFDEF CONDITIONALEXPRESSIONS} { Delphi 6+ }
  {$DEFINE SGDEL_2} // Borland Delphi 2 (C++Builder 1 ??)
  {$DEFINE SGDEL_3} // Borland Delphi 3.0; Borland C++Builder 3.0
  {$DEFINE SGDEL_4} // Borland Delphi 4.0; // Borland C++Builder 4.0
  {$DEFINE SGDEL_5} // Borland Delphi 5.0 & C++Builder 5
  {$DEFINE SGDEL_6} // Borland Delphi 6.0 & C++Builder 6
  {$IF CompilerVersion >= 15.0}
    {$DEFINE SGDEL_7}   // Borland Delphi 7.0
    {$IF CompilerVersion >= 16.0}
      {$DEFINE SGDEL_8} // Borland Delphi 8.0
      {$IF CompilerVersion >= 17.0}
        {$DEFINE SGDEL_2005} // Borland Delphi 2005
        {$DEFINE SG_INLINE}
        {$IF CompilerVersion >= 18.0}
          {$DEFINE SGDEL_2006} // Borland Developer Studio 2006 (v10.0) VER180
          {$IF CompilerVersion >= 18.5}
            {$DEFINE SGDEL_2007} // CodeGear Delphi 2007 for Win32 (v11.0) VER180 and VER185
            {$IF CompilerVersion >= 20.0}
              {$DEFINE SGDEL_2009} // CodeGear Delphi 2009 and C++ Builder	2009 (v12.0) VER200
              {$IF CompilerVersion >= 21.0}
                {$DEFINE SGDEL_2010} // Embarcadero RAD 2010 (v14.0) VER210
                {$IF CompilerVersion >= 22.0}
                  {$DEFINE SGDEL_XE} // Embarcadero RAD XE (v15.0) VER220
                  {$IF CompilerVersion >= 23.0}
                    {$DEFINE SGDEL_XE2} // Embarcadero RAD XE 2 (v16.0) VER230
                    {$IFDEF CPUX64}
                      {$DEFINE SG_CPUX64}
                    {$ELSE}
                      {$DEFINE SG_CPUX32}
                    {$ENDIF}
                    {$IF CompilerVersion >= 24.0}
                      {$DEFINE SGDEL_XE3} // Embarcadero RAD XE 3 (v17.0) VER240
                      {$IF CompilerVersion >= 25.0}
                        {$LEGACYIFEND ON}//Require $IFEND to close $IF statements
                        {$DEFINE SGDEL_XE4} // Embarcadero RAD XE 4 (v18.0) VER250
                        {$IF CompilerVersion >= 26.0}
                          {$DEFINE SGDEL_XE5} // Embarcadero RAD XE 5 (v19.0) VER260
                          {$IF CompilerVersion >= 27.0}
                            {$DEFINE SGDEL_XE6} // Embarcadero RAD XE 6 (v20.0) VER270
                            {$IF CompilerVersion >= 28.0}
                              {$DEFINE SGDEL_XE7} // Embarcadero RAD XE 7 (v21.0) VER280
                              {$IF CompilerVersion >= 29.0}
                                {$DEFINE SGDEL_XE8} // Embarcadero RAD XE 8 (v22.0) VER290
                                {$IF CompilerVersion >= 30.0}
                                  {$DEFINE SGDEL_10_SEATTLE} // Embarcadero RAD 10 Seattle (v23.0) VER300
                                  {$IF CompilerVersion >= 31.0}
                                    {$DEFINE SGDEL_10_BERLIN} // Embarcadero RAD 10.1 Berlin (v24.0) VER310
                                    {$IF CompilerVersion >= 32.0}
                                      {$DEFINE SGDEL_10_TOKYO} // Embarcadero RAD 10.2 Tokyo (v25.0) VER320
                                      {$IF CompilerVersion >= 33.0}
                                        {$DEFINE SGDEL_10_RIO} // Embarcadero RAD 10.3 Rio (v26.0) VER330
                                        {$IF CompilerVersion >= 34.0}
                                          {$DEFINE SGDEL_10_SYDNEY} // Embarcadero RAD 10.4 Sydney (v27.0) VER340
                                          {$IF CompilerVersion >= 35.0}
                                            {$DEFINE SGDEL_11_ALEXANDRIA} // Embarcadero RAD 11 Alexandria (v28.0) VER350
                                            {$IF CompilerVersion >= 36.0}
                                              {$DEFINE SGDEL_12_ATHENS} // Embarcadero RAD 12 Athens (v29.0) VER360
                                            {$IFEND}
                                          {$IFEND}
                                        {$IFEND}
                                      {$IFEND}
                                    {$IFEND}
                                  {$IFEND}
                                {$IFEND}
                              {$IFEND}
                            {$IFEND}
                          {$IFEND}
                        {$IFEND}
                      {$IFEND}
                    {$IFEND}
                  {$IFEND}
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}

{$ELSE}//CONDITIONALEXPRESSIONS

// Borland Delphi 2
{$IFDEF VER90}
  {$DEFINE SGDEL_2}
{$ENDIF}

// C++Builder 1
{$IFDEF VER93}
  {$DEFINE SGDEL_2} //??
{$ENDIF}

// Borland Delphi 3.0
{$IFDEF VER100}
  {$DEFINE SGDEL_2}
  {$DEFINE SGDEL_3}
{$ENDIF}
// Borland C++Builder 3.0
{$IFDEF VER110}
  {$DEFINE SGDEL_2}
  {$DEFINE SGDEL_3}
{$ENDIF}

// Borland Delphi 4.0
{$IFDEF VER120}
  {$DEFINE SGDEL_2}
  {$DEFINE SGDEL_3}
  {$DEFINE SGDEL_4}
{$ENDIF}
// Borland C++Builder 4.0
{$IFDEF VER125}
  {$DEFINE SGDEL_2}
  {$DEFINE SGDEL_3}
  {$DEFINE SGDEL_4}
{$ENDIF}

// Borland Delphi 5.0 & C++Builder 5
{$IFDEF VER130}
  {$DEFINE SGDEL_2}
  {$DEFINE SGDEL_3}
  {$DEFINE SGDEL_4}
  {$DEFINE SGDEL_5}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$ENDIF}

{$ELSE}//FPC

// free pascal
{$DEFINE SGFPC}
{$DEFINE SG_INLINE}
{$IFDEF VER2}
  {$DEFINE SGDEL_3}
  {$DEFINE SGDEL_4}
  {$DEFINE SGDEL_5}
  {$DEFINE SGDEL_6}
  {$DEFINE SGDEL_7}
  {$IFDEF CPUX64}
    {$DEFINE SG_CPUX64}
  {$ELSE}
    {$DEFINE SG_CPUX32}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER3}
  {$DEFINE SGDEL_3}
  {$DEFINE SGDEL_4}
  {$DEFINE SGDEL_5}
  {$DEFINE SGDEL_6}
  {$DEFINE SGDEL_7}
  {$IFDEF CPUX64}
    {$DEFINE SG_CPUX64}
  {$ELSE}
    {$DEFINE SG_CPUX32}
  {$ENDIF}
{$ENDIF}

{$ENDIF}
