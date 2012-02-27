#!/usr/bin/perl -w

my $sec_etf;

print("TIC,SIC,NAICS,GSECTOR,GGROUP,GIND,GSUBIND,SEC_ETF\n");
while(<>){
    next if ($. < 2);
    chomp;
    my($gvkey,$conm,$tic,$sic,$naics,$linkprim,$liid,$linktype,$linkid,$lpermno,$lpermco,$USEDFLAG,$linkdt,$linkenddt,$GSECTOR,$GGROUP,$GIND,$GSUBIND,$SPCINDCD,$SPCSECCD)= split /,/;
    
    if($GSECTOR eq ""){ $sec_etf="UNKNOWN" }
    if($GSECTOR==10){
	$sec_etf="XLE";
	if($GIND==101020){ $sec_etf="OIH" }
    }elsif($GSECTOR==15){ #lump matls w ind
	$sec_etf="XLI";
    }elsif($GSECTOR==20){
	$sec_etf="XLI";
	if($GGROUP==2030){ $sec_etf="IYT" }
    }elsif($GSECTOR==25){
	$sec_etf="XLY";
	if($GGROUP==2550){ $sec_etf="RTH" }
    }elsif($GSECTOR==30){
	$sec_etf="XLP";
    }elsif($GSECTOR==35){
	$sec_etf="XLV";
    }elsif($GSECTOR==40){
	$sec_etf="XLF";
	if($GGROUP==4040){ $sec_etf="IYR" }
	if($GSUBIND==40101015){ $sec_etf="RKH" }
    }elsif($GSECTOR==45){
	$sec_etf="XLK";
	if($GGROUP==4530){ $sec_etf="SMH" }
	if($GIND==451010){ $sec_etf="HHH" }
    }elsif($GSECTOR==50){ #lump telecom with tech
	$sec_etf="XLK";
    }elsif($GSECTOR==55){
	$sec_etf="UTH";
    }else{
	$sec_etf="UNKNOWN";
    }
    unless($sec_etf eq "UNKNOWN"){
	print("$tic,$sic,$naics,$GSECTOR,$GGROUP,$GIND,$GSUBIND,$sec_etf\n")
    }
}
