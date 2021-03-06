{
$Log: acs_audio.inc,v $
Revision 1.7  2006/08/31 20:10:56  z0m3ie
*** empty log message ***

Revision 1.6  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.3  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:36:26  z0m3ie
*** empty log message ***

Revision 1.6  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.5  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.4  2005/11/27 16:50:34  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

Revision 1.3  2005/10/02 16:51:31  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/18 19:29:00  z0m3ie
more progress on driver handling

}


function GetAudioDeviceInfo(DevID : Integer; OutputDev : Boolean) : TACSDeviceInfo;
var
  WIC : TWaveInCaps;
  i : Integer;
begin
  if OutputDev then
  begin
    if DevID >= OutputChannelsCount then
    raise EACSException.Create(Format(strChannelnotavailable,[DevId]));
  end else
  begin
    if DevID >= InputChannelsCount then
    raise EACSException.Create(Format(strChannelnotavailable,[DevId]));
  end;
  if OutputDev then waveOutGetDevCaps(DevID, @WIC, SizeOf(WIC))
  else waveInGetDevCaps(DevID, @WIC, SizeOf(WIC));
  i := 0;
  while WIC.szPname[i] <> #0 do Inc(i);
  SetLength(Result.DeviceName, i);
  Move(WIC.szPname[0], Result.DeviceName[1], i);
  Result.Formats := [];
  if (WIC.dwFormats and WAVE_FORMAT_1M08) <> 0 then Result.Formats := Result.Formats + [af1M08];
  if (WIC.dwFormats and WAVE_FORMAT_1M16) <> 0 then Result.Formats := Result.Formats + [af1M16];
  if (WIC.dwFormats and WAVE_FORMAT_1S08) <> 0 then Result.Formats := Result.Formats + [af1S08];
  if (WIC.dwFormats and WAVE_FORMAT_1S16) <> 0 then Result.Formats := Result.Formats + [af1S16];
  if (WIC.dwFormats and WAVE_FORMAT_2M08) <> 0 then Result.Formats := Result.Formats + [af2M08];
  if (WIC.dwFormats and WAVE_FORMAT_2M16) <> 0 then Result.Formats := Result.Formats + [af2M16];
  if (WIC.dwFormats and WAVE_FORMAT_2S08) <> 0 then Result.Formats := Result.Formats + [af2S08];
  if (WIC.dwFormats and WAVE_FORMAT_2S16) <> 0 then Result.Formats := Result.Formats + [af2S16];
  if (WIC.dwFormats and WAVE_FORMAT_4M08) <> 0 then Result.Formats := Result.Formats + [af4M08];
  if (WIC.dwFormats and WAVE_FORMAT_4M16) <> 0 then Result.Formats := Result.Formats + [af4M16];
  if (WIC.dwFormats and WAVE_FORMAT_4S08) <> 0 then Result.Formats := Result.Formats + [af4S08];
  if (WIC.dwFormats and WAVE_FORMAT_4S16) <> 0 then Result.Formats := Result.Formats + [af4S16];
  Result.DrvVersion := WIC.vDriverVersion;
  if WIC.wChannels = 1 then Result.Stereo := False else Result.Stereo := True;
end;

procedure WaveOutProc(hwo, Msg : LongWord; Instance : Pointer; Param1, Param2 : LongWord); stdcall;
var
  Audio : TStdAudioOut;
begin
  EnterCriticalSection(CrSecO);
  if Msg = WOM_DONE then
  begin
    Audio := TStdAudioOut(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecO);
end;

procedure WaveInProc(hwi, Msg : LongWord; Instance : Pointer; Param1, Param2 : LongWord); stdcall;
var
  Audio : TStdAudioIn;
begin
  EnterCriticalSection(CrSecI);
  if Msg = WIM_DATA then
  begin
    Audio := TStdAudioIn(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecI);
end;

procedure TStdAudioOut.AddBlockToChain(WH : PWaveHdr);
begin
  WH.lpNext := nil;
  EOC^ := WH;
  EOC := @WH.lpNext;
  Dec(aBlock);
end;

procedure TStdAudioOut.SetDevice;
begin
  if Busy then raise EACSException.Create(strBusy);
  if OutputChannelsCount = 0 then  FBaseChannel := 0 else
  if Ch < OutputChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create(Format(strChannelnotavailable,[Ch]));
end;

procedure TStdAudioOut.Prepare;
var
  WF : TPCMWaveFormat;
begin
  // No exceptions here!
  FInput.Init;
  WF.wf.wFormatTag :=  WAVE_FORMAT_PCM;
  WF.wf.nChannels := FInput.Channels;
  WF.wf.nSamplesPerSec := FInput.SampleRate;
  WF.wBitsPerSample := FInput.BitsPerSample;
  WF.wf.nAvgBytesPerSec := WF.wf.nSamplesPerSec*WF.wBitsPerSample div 8;
  WF.wf.nBlockAlign := WF.wf.nChannels * WF.wBitsPerSample div 8;
  waveOutOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveOutProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  aBlock := 0;
  FBuffer := AllocMem(FBufferSize);
  EOC := @BlockChain;
end;

procedure TStdAudioOut.Done;
var
  Tmp : PWaveHdr;
begin
  if _audio_fd <> -1 then
  begin
    while aBlock > 0 do;
    Tmp := BlockChain;
    while Tmp <> nil do
    begin
      BlockChain := Tmp.lpNext;
      waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
      FreeMem(Tmp.lpData);
      Dispose(Tmp);
      Tmp := BlockChain;
    end;
    EOC := @BlockChain;
    waveOutClose(_audio_fd);
    FreeMem(FBuffer);
    _audio_fd := -1;
  end;
  FInput.Flush;
end;

function TStdAudioOut.DoOutput(Abort : Boolean):Boolean;
var
  Len, i, k, vCoef : Integer;
  P : Pointer;
  P1 : PACSBuffer8;
  P2 : PACSBuffer16;
  Tmp : PWaveHdr;
begin
  // No exceptions Here
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  Tmp := BlockChain;         // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain := Tmp.lpNext;
    waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp := BlockChain;
  end;
  EOC := @BlockChain;
  (* Write more than one block. This is needed for audio sources like
     Vorbis codec that return data in small chunks. *)
  for k := aBlock to FReadChunks do
  begin
    GetMem(P, FBufferSize div FReadChunks);
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, FBufferSize div FReadChunks);
    InputLock := False;
    if Len > 0 then Result := True
    else
    begin
      Result := False;
      FreeMem(P);
      Exit;
    end;
    if FVolume < 255 then
    begin
      vCoef := Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P2 := P;
        for i := 0 to (Len shr 1) -1 do
        P2[i] := P2[i]*vCoef;
      end else
      begin
        P1 := P;
        for i := 0 to Len - 1 do
        P1[i] := P1[i]*vCoef;
      end;
    end;
    WriteBlock(P, Len);
  end;
end;

constructor TStdAudioOut.Create;
begin
  inherited Create(AOwner);
  FBaseChannel := 0;
  FVolume := 255;
  _audio_fd := -1;
  Delay := 6;
  FReadChunks := 8;
  FBufferSize := $8000;
end;

destructor TStdAudioOut.Destroy;
begin
  if _audio_fd <> -1 then WaveOutClose(_audio_fd);
  inherited Destroy;
end;

destructor TStdAudioIn.Destroy;
begin
  waveInClose(_audio_fd);
  inherited Destroy;
end;

procedure TStdAudioIn.OpenAudio;
var
  WF : TPCMWaveFormat;
begin
  WF.wf.wFormatTag :=  WAVE_FORMAT_PCM;
  WF.wf.nChannels := FChan;
  WF.wf.nSamplesPerSec := FFreq;
  WF.wBitsPerSample := FBPS;
  WF.wf.nAvgBytesPerSec := WF.wf.nSamplesPerSec*WF.wBitsPerSample div 8;
  WF.wf.nBlockAlign := WF.wf.nChannels * WF.wBitsPerSample div 8;
  if FOpened = 0 then
    begin
      waveInOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveInProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
    end;
  Inc(FOpened);
end;

procedure TStdAudioIn.CloseAudio;
begin
  if FOpened = 1 then
    begin
      waveInClose(_audio_fd);
      FreeMem(FBuffer);
    end;
  if FOpened > 0 then Dec(FOpened);
end;

function TStdAudioIn.GetBPS : Integer;
begin
  Result := FBPS;
end;

function TStdAudioIn.GetCh : Integer;
begin
  Result := FChan;
end;

function TStdAudioIn.GetSR : Integer;
begin
  Result := FFreq;
end;

procedure TStdAudioIn.Init;
begin
  if Busy then raise EACSException.Create(strBusy);
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  FBusy := True;
  OpenAudio;
  waveInStart(_audio_fd);
  BlockChain := nil;
  FSize := FRecBytes;
  aBlock := 0;
  EOC := @BlockChain;
end;

procedure TStdAudioIn.Flush;
var
  Tmp : PWaveHdr;
begin
  while aBlock > 0 do;  // wait until pending data blocks are put to the chain
  waveInReset(_audio_fd);    // return all pending data blocks
  sleep(10);
  Tmp := BlockChain;         // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain := Tmp.lpNext;
    waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp := BlockChain;
  end;
  CloseAudio;
  FBusy := False;
end;

procedure TStdAudioIn.SetDevice;
begin
  if Busy then raise EACSException.Create(strBusy);
  if Ch < InputChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create(Format(strChannelnotavailable,[Ch]));
end;

function TStdAudioIn.GetData(Buffer : Pointer; oBufferSize : Integer): Integer;
var
  Tmp : PWaveHdr;
begin
  if not Busy then  raise EACSException.Create(strStreamnotopen);
  if FRecBytes >= 0 then
    if (FPosition >= FRecBytes) then
      begin
        Result := 0;
        Exit;
      end;
  while aBlock < FBlocksCount do
    NewBlock;
  if BufStart > BufEnd then
    begin
      BufStart := 1;
      while BlockChain = nil do
        sleep(10);
      TMP := BlockChain;
      BlockChain := BlockChain.lpNext;
      if BlockChain = nil then
        EOC := @BlockChain;
      Move(Tmp.lpData[0],  FBuffer[1], Tmp.dwBytesRecorded);
      BufEnd := Tmp.dwBytesRecorded;
      waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
      FreeMem(Tmp.lpData);
      Dispose(Tmp);
    end;
  if BufferSize < (BufEnd - BufStart + 1) then
    Result := BufferSize
  else
    Result := BufEnd - BufStart + 1;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TStdAudioOut.WriteBlock;
var
  WH : PWaveHdr;
begin
  Inc(aBlock);
  New(WH);
  WH.lpData := P;
  WH.dwBufferLength := Len;
  WH.dwLoops := 0;
  WH.dwFlags := 0;
  waveOutPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveOutWrite(_audio_fd, WH, SizeOf(TWaveHdr));
end;

procedure TStdAudioIn.NewBlock;
var
  WH : PWaveHdr;
begin
  New(WH);
  GetMem(WH.lpData, BufferSize div FBlocksCount);
  WH.dwBufferLength := BufferSize div FBlocksCount;
  WH.dwFlags := 0;
  waveInPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveInAddBuffer(_audio_fd, WH, SizeOf(TWaveHdr));
  Inc(aBlock);
end;

function CountChannels : Integer;
begin
  OutputChannelsCount := waveOutGetNumDevs;
  InputChannelsCount := waveInGetNumDevs;
end;

procedure TStdAudioIn.AddBlockToChain(WH : PWaveHdr);
begin
  WH.lpNext := nil;
  EOC^ := WH;
  EOC := @WH.lpNext;
  Dec(aBlock);
end;

