BeginPackage["Plotters`"]

DomainColourPlot::usage = "Computes domain colouring plot of given complex function."

Options[DomainColourPlot] =
	{
		AspectRatio -> Automatic,
		ImageSize -> Automatic,
		PlotPoints -> 100,
		ImagePadding -> All
	}

Begin["Private`"]

DomainColourPlot[f_, {xMin_, xMax_}, {yMin_, yMax_}, OptionsPattern[]] :=
	RegionPlot[True,
		{x, xMin, xMax}, {y, yMin, yMax}, 
		ColorFunction -> 
			Function[{x, y}, Hue[
				Arg[f[x + I y]]/(2 \[Pi]),
				1/(1 + 0.3 Log[Abs[f[x + I y]] + 1]), 
				1 - 1/(1.1 + 5 Log[Abs[f[x + I y]] + 1])
			]], 
		ColorFunctionScaling -> False,
		PlotPoints -> OptionValue[PlotPoints], 
		AspectRatio ->
			If[SameQ[OptionValue[AspectRatio], Automatic],
				(yMax - yMin) / (xMax - xMin),
				OptionValue[AspectRatio]],
		ImageSize -> OptionValue[ImageSize],
		ImagePadding -> OptionValue[ImagePadding]
	]

End[]

EndPackage[]
