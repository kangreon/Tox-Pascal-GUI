{$IFDEF FPCUNIX}
  {$R Resource\images.rc}
{$ENDIF}

{$IFDEF FPCWIN}
  {$R Resource\tox-res.rc}
  {$R Resource\images.rc}
{$ENDIF}

{$IFDEF DELPHI}
  {$R 'images.res' 'Resource\images.rc'}
  {$R 'tox-res.res' 'Resource\tox-res.rc'}
{$ENDIF}