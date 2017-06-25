# The start shell of Glob
# This script will work in PowerShell 5 of higher.

function launchFromURL($url)
{
    $cfg_path = getFromURL($url);
    runGlob($cfg_path);
}


# To get the config
function getFromURL ($url)
{
    if ( -not (Test-Path "C:\ProgramData\Glob"))
    {
        New-Item  "C:\ProgramData\Glob" -ItemType Directory ;
    }
	$response = Invoke-WebRequest -Uri $url -Method Get;
	if  ($response.StatusCode -eq 200)
	{
		Write-Verbose ("Get the config file from"+$url);
		$ConfigText =  $response.Content;
		Write-Verbose $ConfigText;
		echo "$ConfigText" | Out-File -filepath 'C:\ProgramData\Glob\config' -Encoding utf8;
		return "C:\ProgramData\Glob\config";
	}
	else
	{
		Write-Error "Can not get the config";
		Write-Error $response;
	}
}

function getFromEnv ($url)
{

}

# To run the Glob
function runGlob($cfg_path)
{
	if ( -not (getGlob "glob-launch" $cfg_path))
	{
		if ( (Test-Path 'C:\Program Files\Glob\glob-launch.*') -or (Test-Path 'C:\Program Files\Glob\glob.*') -or (Test-Path 'C:\Program Files\Glob\glob-start.*'))
		{
			$env:Path  = $env:Path+";C:\Program Files\Glob";
			$status = (getGlob "glob-launch" $cfg_path) -or (getGlob "glob" $cfg_path) -or (getGlob "glob-start" $cfg_path);
			if (-not $status)
			{
				Write-Error "Can not start.";
			}
			return;
		}
	}
}
function getGlob($command,$cfg_path)
{
	$glob_path =  Get-Command $command;
	if ($glob_path.Length -gt 0)
	{
		$gp = $glob_path[0].Source;
		Write-Verbose ("Find glob in " + $gp);
		runItem $gp $cfg_path;
		return True;
	}
	return False;
}
function runItem($glob_path,$cfg_path)
{
	& $glob_path -f $cfg_path;
}