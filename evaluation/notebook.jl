### A Pluto.jl notebook ###
# v0.20.4

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    #! format: off
    quote
        local iv = try Base.loaded_modules[Base.PkgId(Base.UUID("6e696c72-6542-2067-7265-42206c756150"), "AbstractPlutoDingetjes")].Bonds.initial_value catch; b -> missing; end
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : iv(el)
        el
    end
    #! format: on
end

# ╔═╡ 006bac1e-2965-4c35-b243-1035418c4a48
begin
	using PlutoUI
	using DataFrames
	using HypothesisTests
	using TimeSeries
	using MixedModels
	using FreqTables
	using StatsKit
	using SwarmMakie
	using CairoMakie
	using AlgebraOfGraphics
	using LaTeXStrings
	using Printf: @sprintf, @printf
	using PlutoLinks: @ingredients
end

# ╔═╡ 974ac923-a511-4128-ad12-b75ccdacec8c
PlutoUI.TableOfContents(include_definitions=false)

# ╔═╡ 93a5e415-4b56-4edd-b59c-c00ce4be8612
md"""
# Argus Evaluation
"""

# ╔═╡ d8364382-e421-4be6-9854-ab43cdfc0b9e
md"""
### Initializing packages

*When running this for the first time, this could take a while (~10--15 minutes). Hang in there!*
"""

# ╔═╡ a85aa103-5f00-4d3a-808a-be678b64500a
md"""
*Note,* for a really annoying reason, Pluto isn't good at importing other Pluto notebooks. The best thing to do right now is to evaluate the file, saving the output in this variable λ as if it *were* a module. The only downside is that you can't `import` all symbols from the module, so we have to qualify things with ‘λ.’
"""

# ╔═╡ 76edb6c5-01aa-4457-b340-876663bd91ac
 λ = @ingredients("./EvalUtil.jl");

# ╔═╡ bbdb9c4b-9946-4563-899a-db559357787c
form = λ.cel("Responses");

# ╔═╡ 904eb0d6-aba7-43d5-9032-e910f80700bd
versions = λ.cel("Meta");

# ╔═╡ 886642fd-ab1d-480c-b204-86909a7193c3
tasks = λ.read_tasks();

# ╔═╡ 6fa2360d-cae0-4d1d-83fb-9cca1d471514
md"""
### Data preprocessing
"""

# ╔═╡ 09b57e4d-7400-47ee-9981-f6f39eb128f1
md"""
Functional helpers for DataFrames
"""

# ╔═╡ 28aa8578-f4c1-4215-9fab-e44193073dba
begin
	sel(ARGS...) = f -> DataFrames.select(f, ARGS...)
	pipe(ARGS...) = f -> DataFrames.transform(f, ARGS...)
	comb(ARGS...) = f -> DataFrames.combine(f, ARGS...)
	grouped(ARGS...) = f -> DataFrames.groupby(f, ARGS...)
	ftask(s::AbstractString) = df -> filter(row -> row.name == s, df)
	filt(func) = df -> DataFrames.filter(func, df)
	sort_by(v) = df -> sort(df, v)
	mp(s, f) = pipe(s => ByRow(f) => s)
	with_argus = filt(row -> row.has_argus)
	no_argus = filt(row -> !row.has_argus)
	labelize(df) = (
		df 
		|> mp(:has_argus, λ.argus_label)
		|> mp(:localized, s -> Second(s).value)
		|> mp(:duration, s -> Second(s).value)
		|> mp(:fixduration, s -> Second(s).value)
		|> mp(:is_typestate, λ.ts_label)
		|> mp(:is_real, λ.rw_label)
		|> mp(:has_async_experience, λ.async_label)
		|> mp(:axum, λ.familiar_label)
		|> mp(:is_familiar, λ.familiar_label)
		|> mp(:name, λ.task_label)
	)
end

# ╔═╡ abc0d0ea-27e6-4bad-bc95-9d6539ab4f44
"""
Join a task's DataFrame with the rest of the study information (version number, form responses, etc). This function also *cleans* and *infers* the data to be used in the 
analysis.
"""
study_info(df) = (
	df # Join the data with the versions metadata and form responses
	|> (f -> DataFrames.outerjoin(df, versions; on=:id))
	|> (f -> DataFrames.outerjoin(f, form; on=:id))

	# "Remove" missings from data. There isn't data missing, but Julia
	# thinks there is because of how the data is read in. 
	|> pipe(:name => ByRow(n -> coalesce(n, "DUMMY")) => :name)
	|> pipe(:start => ByRow(x -> coalesce(x, λ.TIME_ZERO)) => :start)
	
	# Some participants took a break in the middle of a task to do something, e.g., answer the door/phone, left the room to get water. The fields `break_start` and `break_end` are used for these scenarios (`missing` otherwise).
	
	|> pipe(Cols(:break_start, :end) => ByRow(coalesce) => :break_start)
	|> pipe(Cols(:break_end, :end) => ByRow(coalesce) => :break_end)
	|> pipe(Cols(:break_end, :break_start) => (.-) => :break_duration)
	
	# The ending time `end` will become `end = end - (break_end - break_start)`.
	|> pipe(Cols(:start, :end, :break_duration) => 
		ByRow((s, e, bd) ->
		if ismissing(e)
			s + λ.MAX_TASK_DURATION
		else 
			e - bd
		end) => :end)
	
	# If `found_rc` came *before* the break, then we don't change it, if `found_rc` came after the break, then we shift it down by `break_duration`.
	|> pipe(Cols(:found_rc, :break_start, :break_duration, :end) => 
		ByRow((frc, bs, bd, e) -> 
		if ismissing(frc)
			e
		elseif frc < bs 
			frc 
		else 
			frc - bd
		end) => :found_rc)
	
	# Replace the "y"/"n" from the spreadsheet with Bool
	|> pipe("has-argus" => ByRow(s -> !ismissing(s) && s == "y") => :has_argus)
	
	# Did the participant successfully solve the task?
	|> pipe(:result => ByRow(v -> !ismissing(v) && v == "s") => :is_success)
	
	# Did the participant unsuccessfully finish?
	|> pipe(:result => ByRow(v -> !ismissing(v) && v == "f") => :is_incorrect)

	# Did the participant give up?
	|> pipe(:result => ByRow(v -> !ismissing(v) && v == "q") => :did_quit)


	# NOTE: some participants got a wee bit of extra time. This usually happened because I forgot about the time, they seemed close to an answer, or they were talking and I didn't want to interrupt. However, we need to correct for this.
	# `end` needs to be clamped to `start + MAX_TASK_DURATION`.
	# `is_success` can flip from `true` to `false` if the `end` time is greater than the maximum allotted duration.
	# `found_rc` needs to be clamped to `start + MAX_TASK_DURATION`.
	|> pipe(Cols(:start, :found_rc, :end, :is_success) => 
		ByRow((s,frc,e,is) -> begin
			MAX_END = s + λ.MAX_TASK_DURATION
			new_end = if e > MAX_END MAX_END else e end
			new_frc = if MAX_END < frc MAX_END else frc end
			new_is = if MAX_END < e false else is end
			[new_frc, new_end, new_is]
		end) => [:found_rc, :end, :is_success])

	## ############### ##
	## Done Sanitizing ##
	## ############### ##

	|> pipe(Cols(:end, :start) => (.-) => :duration)
	|> pipe(:duration => ByRow(t -> Dates.value.(t)/1e9) => :durationint)
	|> pipe(Cols(:found_rc, :start) => (.-) => :localized)
	|> pipe(Cols(:end, :found_rc) => (.-) => :fixduration)
	|> pipe(:localized => ByRow(t -> Dates.value.(t)/1e9) => :localizedint)
	|> pipe(Cols(:end, :found_rc) => (.!=) => :did_localize)
	
	# Is this problem real or synthetic?
	|> pipe(:name => ByRow(n -> n in λ.real_world_problems) => :is_real)
	|> pipe(:is_real => ByRow((!)) => :is_synthetic)

	# Is this problem a typestate problem or a "tree" problem?
	|> pipe(:name => ByRow(n -> n in λ.tree_problems) => :is_tree)
	|> pipe(:is_tree => ByRow((!)) => :is_typestate)

	# Make column for familiarity with a crate
	|> λ.crate_familiarity("Axum")
	|> λ.crate_familiarity("Bevy")
	|> λ.crate_familiarity("Diesel")
	
 	# Is the developer familiar with the underlying crate?
 	|> pipe(Cols(:name, :axum, :bevy, :diesel) => 
		ByRow(λ.is_familiar) => :is_familiar)

	# How many years total have you been programming?
	|> pipe(r"years total" => ByRow(identity) => :years)
	
	# Are you a Rust expert?
	|> pipe(r"expert" => ByRow(r -> r == "Yes") => :identifies_as_expert)

	|> pipe(r"async Rust" => ByRow(r -> !ismissing(r) && r == "Yes") => :has_async_experience)
	
	# Are you employed to use Rust
	|> pipe(r"employed" => ByRow(r -> r == "Yes") => :is_rust_employed)

	# How long have you used Rust?
	|> pipe(r"used Rust" => ByRow(identity) => :rust_experience)
	|> pipe(:rust_experience => ByRow(x -> coalesce(x, 0)) => :rust_experience)

	# Have you used Rust for enough years?
	|> pipe(:rust_experience => ByRow(x -> x >= λ.EXPERT_RUST_EXP) => :enough_years)

	# Are an expert if they self-identified or have enough rust experience
	|> pipe(Cols(:identifies_as_expert, :enough_years) => (.|) => :is_expert)
	
	# Quantify their "Type Class" experience by summing Haskell/Lean experience
	|> pipe(Cols(r"Haskell", r"Lean") => 
		ByRow(λ.type_class_experience) => :type_class_experience)
)

# ╔═╡ 60e21bd4-779e-440b-8a7a-805eca8690c5
md"""
### Study Meta Data
"""

# ╔═╡ f1ffa2d0-0449-4f28-af5e-25399e5eef99
SIGNED_UP = length(versions.id)

# ╔═╡ 69a74948-61ff-498b-a6e5-260a3c40d2b0
DID_PARTICIPATE = SIGNED_UP - count(ismissing, versions.version)

# ╔═╡ 1dece7d4-b4a4-4c1d-a5b2-81f1e14716c0
PARTICIPATION_RATE = DID_PARTICIPATE / SIGNED_UP

# ╔═╡ 59dcf3ba-53fa-448b-b8de-25f981ee7d5f
OFFICIAL_STUDY_VERSION = 3

# ╔═╡ 9b527f31-41c3-4dad-bc71-a7ff9fa03ddc
@bind version Select(
	versions.version |> skipmissing |> unique |> sort, 
	default=OFFICIAL_STUDY_VERSION
)

# ╔═╡ e5d38326-108b-4c4d-ad77-5be3597123c5
"""
Filter the provided DataFrame by participants involved in version `version` of the study.
"""
pick_version = filt(row -> !ismissing(row.version) && row.version == version)

# ╔═╡ a10c2e29-aa43-4384-9cc9-ea156806a9ab
by_task = @. study_info(tasks) |> pick_version;

# ╔═╡ edefac7e-d9b7-4302-8b53-b451d8115498
all_tasks = reduce(vcat, by_task);

# ╔═╡ 8b7ae46c-fc53-4759-b3de-81584f43723c
Markdown.parse("""
You can interact with the page by binding the dropdown input to a variable. This controls which *version* of the study the following data analyzes. You're currently looking at **version `$version`**. 
""")

# ╔═╡ a08c1ce2-a892-418a-9d4a-d5208afa78c4
Markdown.parse("""
### Recruiting Information

Of the `$SIGNED_UP` people who signed up to participate, `$DID_PARTICIPATE` showed up. 

This makes for a *participation rate* of

	$DID_PARTICIPATE / $SIGNED_UP = $PARTICIPATION_RATE
""")

# ╔═╡ fdb2b9af-d1db-49d6-96de-7cee8a99784c
if version != 3
	Markdown.parse("""
	> ⚠️ **WARNING** ⚠️
	>
	>You are not looking at the results for the official Argus evaluation.
	>
	> Official version: `$OFFICIAL_STUDY_VERSION`
	>
	> Current version: `$version`
	""")
else nothing end

# ╔═╡ bb5ec6d8-85b0-4cbd-b7bf-f259b3ed0ca1
function outcome_probability(df, counting; groupby=:has_argus)
	(df 
	|> grouped(groupby)
	|> comb(counting => sum => :count, nrow)
	|> pipe(Cols(:count, :nrow) => (./) => :percent)
	|> pipe(Cols(:count, :nrow, :percent) => ByRow((c,nr,p) -> begin
		lower,upper = confint(BinomialTest(c, nr))
		return lower, upper
	end) => :confint)
	|> pipe(:confint => [:confint_low, :confint_high]))
end

# ╔═╡ 1713a0fc-a039-428e-b565-194e47055596
outcome_probability(s::Symbol; groupby=:has_argus) = 
	df -> outcome_probability(df, s; groupby=groupby)

# ╔═╡ b5c9beb8-c42a-43ab-b052-38c4f42f46b4
const agdata = AlgebraOfGraphics.data

# ╔═╡ a31295ec-e760-458d-9097-6cf934653301
const agdraw! = AlgebraOfGraphics.draw!

# ╔═╡ 181dc4fa-12cb-432a-903f-2d8199151eb1
LibertineFont = if haskey(ENV, "LIBERTINE_PATH") 
	((ENV["LIBERTINE_PATH"] * "/truetype/public/LinLibertine_R.ttf") 
	|> Makie.to_font)
else
	"Helvetica"
end

# ╔═╡ 7be514fb-bd0d-40a6-878f-3eb61f60d1fd
md"""
Update the plot styles to be maximally visible in the paper, and use Linux Libertine R to match the body text.
"""

# ╔═╡ ce0d1b8b-39cb-4843-ba67-6ac1aad2feea
CairoMakie.activate!()

# ╔═╡ 5921272a-a993-4b43-9137-fd06f0c4734f
update_theme!(
	fonts=(; regular=LibertineFont, weird=LibertineFont),
	fontsize=32,
	axis=(; topspinevisible=false, rightspinevisible=false,),
	linewidth=4,
)

# ╔═╡ 686b24b0-78a9-42b5-9d1d-845b8ffe0258
md"""
# 5. Evaluation
"""

# ╔═╡ 1cd020b9-dd14-42ce-8dbc-59a3ff3377e6
md"""
## 5.1.1. Methodology
"""

# ╔═╡ d555960f-0147-478d-8b94-da0880e40ae2
md"""
#### Participants
"""

# ╔═╡ 7f4591ea-ec7a-4d9a-a421-20fb4b8b977a
N = unique(all_tasks.id) |> length

# ╔═╡ f3164cd7-9970-4b28-9585-f52b6cc66d3f
Markdown.parse("""
# User Study V`$version` (N=`$N`)
""")

# ╔═╡ 67277703-4a0b-4ffc-a00b-aec728b73f37
md"""
We recruited N = $N participants
"""

# ╔═╡ c13f6c71-1831-4d26-a726-f61b46ad7b78
function report_min_max_median(v; title="Min/Max/Median")
	m = median(v)
	mi = minimum(v)
	mx = maximum(v)
	
	
	Markdown.parse("""
	$title
	- Median $m
	- Min $mi
	- Max $mx
	""")
end; 

# ╔═╡ aa687140-0390-444d-bca4-f35b995decb8
report_min_max_median(all_tasks.years; title="Years Programming")

# ╔═╡ b938307f-1431-4062-8f21-f1d839de3aab
report_min_max_median(all_tasks.rust_experience; title="Years w/ Rust")

# ╔═╡ b97d840c-0d89-481b-82f1-1a85c841163b
md"""
#### Analysis
"""

# ╔═╡ 3f2ceb2a-a14a-4502-8bf0-2d311d4d4062
"""
The participant IDs used for IRR.
"""
IRR_IDS = [19, 74, 62, 38, 50];

# ╔═╡ ae17e013-c5fe-46b6-856a-a9e30ddeef89
"""
Compute the Spearman Rho correlation value for localization times.
"""
function compute_irr(; version=1)
	function inner(dfs)		
		elaborate(df, i) = (
			dfs[i]
			|> study_info
			|> sel(Cols(:id, :found_rc)) 
			|> pipe(:id => ByRow(r -> i) => :task)
		)
		
		return (
			map(i -> elaborate(dfs[i], i), 1:4) 
			|> dfs -> reduce(vcat, dfs)
			|> mp(:found_rc, r -> convert(Second, r - λ.TIME_ZERO).value)
			|> filt(row -> row.id in IRR_IDS) 
			|> sort_by([:id, :task])
		)
	end

	dir = "irr-snapshots/" * string(version) * "/"
	gavins_tasks = λ.read_tasks(prefix=dir)
	wills_tasks = λ.read_tasks(prefix=dir * "Will's Copy of ")
	
	gavins_irr = inner(gavins_tasks)
	wills_irr = inner(wills_tasks)

	(
		corspearman=corspearman(gavins_irr.found_rc, wills_irr.found_rc),
		meanad=StatsBase.meanad(gavins_irr.found_rc, wills_irr.found_rc)
	)
end;

# ╔═╡ 7d536e79-d41c-4797-9484-a68002fcf4c7
IRR_RESULTS = compute_irr(version=1)

# ╔═╡ aff33a67-33d6-4538-b797-a517d3346507
IRR_CORRELATION = IRR_RESULTS.corspearman

# ╔═╡ f63d0de8-681e-4f79-b73e-9dd6bb765715
IRR_MEANAD = IRR_RESULTS.meanad

# ╔═╡ a37fc0b7-0a1a-456c-9b59-4429a8913afc
md"""
The inter-rater reliability correlation ρ=$IRR_CORRELATION as computed by the function `compute_irr` above. *(Hidden by default click the eye icon to the left of the cell to toggle visiblity.)*

The mean absolute difference *meanad*=$IRR_MEANAD seconds
"""

# ╔═╡ 37564825-7961-4ce0-9c23-1481a8c445c2
md"""
## 5.1.2. Results
"""

# ╔═╡ 9e5c5e5e-aef7-4159-ab87-999d16a8279f
begin
	box_plot_axis=(
		xlabel="Has Argus",
		limits = (nothing, (0, 610)),
		yticks=0:60:600,
		xgridvisible=false,
		rightspinevisible = false,
    	topspinevisible = false
	)

	bar_plot_axis = (
		limits=(nothing, (0, 1.0)),
		xlabel="Has Argus",
		yticks=0:0.2:1,
		xgridvisible=false,
		rightspinevisible = false,
    	topspinevisible = false
	)

	plt_thres = mapping(600) * visual(HLines; color=:red, label="Time Threshold");
	
end; 

# ╔═╡ 8a226c19-6749-44a8-99c1-1a5653dd41a2
md"""
### Figure 10
"""

# ╔═╡ 773f817c-f1eb-4cfd-ae9a-61572b84911e
function fig9_a()
	labeled = all_tasks |> labelize
	df1 = labeled |> outcome_probability(:is_success) |> sort_by(:has_argus)

	fig = Figure(size=(400, 400))

	d = agdata(df1)
	
	m1 = mapping(
		:has_argus, 
		:percent,
		color=:has_argus=>""
	)
	
	m2 = mapping(
		:has_argus,
		:confint_low,
		:confint_high,
	)
	
	p_bars = d * m1 * visual(BarPlot)
	p_err = d * m2 * visual(Rangebars) 

	subplt = agdraw!(fig[1, 1], p_bars + p_err; axis=(;
		ylabel="Fix Rate",
		bar_plot_axis...
	))

	fig
end;

# ╔═╡ 0db6734d-a0e8-4072-b3f6-7ebb602b8d02
function fig9_b()
	labeled = all_tasks |> labelize
	
	fig = Figure(; size=(400, 400))

	d = agdata(labeled)
	m = mapping(:has_argus, :duration; color=:has_argus)
	v = visual(BoxPlot)
	plt_box = d * m * v 
	
	subplt=agdraw!(
		fig,
		plt_thres + plt_box;
		axis=(; 
			ylabel="Fix Time (s)",
			box_plot_axis...
		),
	)
	
	fig
end;

# ╔═╡ 6ad727af-d324-4cb0-97b9-f574a173c164
FIG10_C = fig9_a()

# ╔═╡ d72fdc5d-fa81-4a70-9024-dd2d092c3fdd
FIG10_D = fig9_b()

# ╔═╡ d7db3e77-236c-4d36-b7df-aeb136f16a1d
function fig10_a()
	labeled = all_tasks |> labelize
	df1 = labeled |> outcome_probability(:did_localize) |> sort_by(:has_argus)

	fig = Figure(size=(400, 400))

	d = agdata(df1)
	
	m1 = mapping(
		:has_argus, 
		:percent,
		color=:has_argus
	)
	
	m2 = mapping(
		:has_argus,
		:confint_low,
		:confint_high,
	)
	
	p_bars = d * m1 * visual(BarPlot)
	p_err = d * m2 * visual(Rangebars) 
	agdraw!(fig[1, 1], p_bars + p_err; axis=(;
		ylabel="Localization Rate",
		bar_plot_axis...
	))

	fig
end;

# ╔═╡ 07de8b5b-00b8-4150-9cb1-fc5557e8d648
FIG10_A = fig10_a()

# ╔═╡ 5dbb3baa-6178-4d6c-aa91-ecae89f4f6ac
function fig10_b()
	labeled = all_tasks |> labelize

	fig = Figure(; size=(400, 400))

	d = agdata(labeled)
	m = mapping(:has_argus, :localized; color=:has_argus)
	v = visual(BoxPlot)
	plt_box = d * m * v 
	
	subplt=agdraw!(
		fig,
		plt_thres + plt_box; 
		axis=(; 
			ylabel="Localization Time (s)",
			box_plot_axis...

		),
	)

	fig
end;

# ╔═╡ 84f54ff6-4ff3-4b32-be3b-6e4a6a9350cc
FIG10_B = fig10_b()

# ╔═╡ f2d58bb1-39e4-49f1-baa6-e2c8ac7979c2
md"""
---
"""

# ╔═╡ 23ac280a-d127-4dd7-812b-f7c4b07ee28d
md"""
### RQ1
"""

# ╔═╡ 22fe4993-0efa-4cbe-8ff3-4354d3c3378e
function time_outcomes_latex(df; prefix="", percents=false)
	as_percent = v -> if percents floor(v * 100)|>Int else v end
	rrr = v -> round(v, sigdigits=2) |> v -> as_percent(v)

	with_argus_p_localized = with_argus(df).percent[1] |> rrr
	no_argus_p_localized = no_argus(df).percent[1] |> rrr

	
	with_argus_ci_lo = with_argus(df).confint_low[1] |> rrr
	with_argus_ci_hi = with_argus(df).confint_high[1] |> rrr
	
	no_argus_ci_lo = no_argus(df).confint_low[1] |> rrr
	no_argus_ci_hi = no_argus(df).confint_high[1] |> rrr
	
	L"""
	% With/No Argus Success rate for %$prefix
	\newcommand{\WithArgus%$prefix}{%$with_argus_p_localized}
	\newcommand{\NoArgus%$prefix}{%$no_argus_p_localized}
	% With Argus CI for %$prefix
    \newcommand{\WithArgusCILo%$prefix}{%$with_argus_ci_lo}
	\newcommand{\WithArgusCIHi%$prefix}{%$with_argus_ci_hi}
	% No Argus CI for %$prefix
	\newcommand{\NoArgusCILo%$prefix}{%$no_argus_ci_lo}
	\newcommand{\NoArgusCIHi%$prefix}{%$no_argus_ci_hi}
	"""
end;

# ╔═╡ b3198067-2518-40cb-a2d6-c78ca87b54e8
ChisqTest(freqtable(all_tasks.did_localize, all_tasks.has_argus))

# ╔═╡ ad33d56d-c4eb-446e-94b2-0e1fde12433f
LOCALIZE_OUTCOMES = outcome_probability(all_tasks, :did_localize)

# ╔═╡ 9be26989-da51-476a-ba42-deb2e2c0d54c
WITH_ARGUS_LOCALIZED_RATE = with_argus(LOCALIZE_OUTCOMES).percent[1]

# ╔═╡ 28f17b18-f527-49e1-83f9-6962e688693a
WITH_ARGUS_LOCALIZED_RATE_CI = with_argus(LOCALIZE_OUTCOMES).confint[1]

# ╔═╡ 4142602d-c97e-474c-95fb-628e7d821fcc
NO_ARGUS_LOCALIZED_RATE = no_argus(LOCALIZE_OUTCOMES).percent[1]

# ╔═╡ cf3a3688-26f7-4870-86e7-06266ccd27ef
NO_ARGUS_LOCALIZED_RATE_CI = no_argus(LOCALIZE_OUTCOMES).confint[1]

# ╔═╡ 8ebdbc8c-4b63-40ba-99fa-868196785854
LOCALIZED_PP = WITH_ARGUS_LOCALIZED_RATE - NO_ARGUS_LOCALIZED_RATE

# ╔═╡ 4e9ed283-274c-4d43-9d99-ebf90ab67501
LOCALIZED_RATE_FACTOR = (WITH_ARGUS_LOCALIZED_RATE / NO_ARGUS_LOCALIZED_RATE) |> v -> round(v, sigdigits=2)

# ╔═╡ 9840aae1-fcc0-41fd-b3e7-3c2e52d0571e
md"""
#### Localization rate

- Localization with Argus

  $WITH_ARGUS_LOCALIZED_RATE (CI = $WITH_ARGUS_LOCALIZED_RATE_CI)

- Localization without Argus

  $NO_ARGUS_LOCALIZED_RATE (CI = $NO_ARGUS_LOCALIZED_RATE_CI)

A difference of $LOCALIZED_PP pp or $LOCALIZED_RATE_FACTOR x.

Using the below Chi-square test, this effect is statistically significant. *(X(1, 100) = 22.236233 shown below under `Details`)*
"""

# ╔═╡ 4b690c44-0cdc-41aa-b22d-3b4c4878a31f
L"""
\newcommand{\LocalizedRateFactor}{$%$LOCALIZED_RATE_FACTOR\times$}
""" |> println

# ╔═╡ 0111bedd-7c8c-419e-aafc-e7b3181dab1a
time_outcomes_latex(LOCALIZE_OUTCOMES; prefix="Localize", percents=true) |> println

# ╔═╡ ec41ea88-708e-4b0a-a995-5f4f356b1f18
md"""
---
#### Localization times
"""

# ╔═╡ c85e3b88-aeef-447c-b47e-7e6660d9c8ca
KruskalWallisTest(with_argus(all_tasks).localizedint, no_argus(all_tasks).localizedint)

# ╔═╡ 5d095859-f032-4b8c-bfc1-441c91ec7fb8
WITH_ARGUS_LOCALISED_MD = with_argus(all_tasks).localizedint |> median |> round |> s -> canonicalize(Second(s))

# ╔═╡ 271eea30-91c6-4204-9958-e59b241f355a
WITH_ARGUS_LOCALIZED_CI = SignTest(with_argus(all_tasks).localizedint) |> confint |> t -> map(s -> canonicalize(Second(s)), t)

# ╔═╡ 922bc3cc-e4b8-4073-87c1-a2d101c7eed5
md"""
Median localization time *without* Argus 
"""

# ╔═╡ c8dad05d-3e21-435a-a3d9-4d4e0bc389c0
NO_ARGUS_LOCALIZED_MD = no_argus(all_tasks).localizedint |> median |> round |> s -> canonicalize(Second(s))

# ╔═╡ 35a6155e-69d0-46fa-9515-45c2a72aafc4
NO_ARGUS_LOCALIZED_CI = SignTest(no_argus(all_tasks).localizedint) |> confint |> t -> map(s -> canonicalize(Second(s)), t)

# ╔═╡ 1901b368-a426-4027-92e7-e0942abd9e82
LOCALIZED_DIFF = NO_ARGUS_LOCALIZED_MD - WITH_ARGUS_LOCALISED_MD

# ╔═╡ 5b0ce1fb-ca29-497f-be42-c5e1a2462eaa
TIMES_LOC = (
	(no_argus(all_tasks).localizedint |> median) / (with_argus(all_tasks).localizedint |> median)
) |> d -> round(d, sigdigits=2)

# ╔═╡ 41d674c9-2cf5-43c8-887c-19b2caf10321
md"""
- Localization time with Argus

  median $WITH_ARGUS_LOCALISED_MD (CI = $WITH_ARGUS_LOCALIZED_CI)

- Localization time without Argus

  median $NO_ARGUS_LOCALIZED_MD (CI = $NO_ARGUS_LOCALIZED_CI)

Difference of $LOCALIZED_DIFF pp or $TIMES_LOC x

Using a Kruskal-Wallis test this effect is statistically significat *(X(1, 100) = 31.3892 shown below under `Details`.)*
"""

# ╔═╡ 7241f6dd-43d8-438a-bc2b-7515c0f7b7cc
L"""
\newcommand{\LocalizedTimeFactor}{$%$TIMES_LOC\times$}
""" |> println

# ╔═╡ 48384a51-2f58-4d94-893f-9cab73d29789
md"""
---
#### Fix Rate
"""

# ╔═╡ 10487a7f-1ef4-496e-a48f-1149a1c0cf08
fit(
    MixedModel, 
    @formula(is_success ~ has_argus + (1 | id)),
    all_tasks,
    Binomial(),
)

# ╔═╡ 769382aa-3666-402a-a6c8-5b934ce6c69d
FIX_RATE = outcome_probability(all_tasks, :is_success)

# ╔═╡ 8f6826a5-b8f9-40b4-a793-a67028c6dea5
WITH_ARGUS_FIX_RATE = with_argus(FIX_RATE).percent[1]

# ╔═╡ e8ed396a-a0a3-4080-9cf5-0b5402253cb5
WITH_ARGUS_FIX_CI = with_argus(FIX_RATE).confint[1]

# ╔═╡ 4c0fcdf3-4df4-463b-ab53-538e9c3fd9f9
NO_ARGUS_FIX_RATE = no_argus(FIX_RATE).percent[1]

# ╔═╡ e183e449-d112-4317-b987-84f5980cfc65
NO_ARGUS_FIX_CI = no_argus(FIX_RATE).confint[1]

# ╔═╡ 646d39a4-b107-4b6b-b667-842bcd25e502
FIX_RATE_PP = WITH_ARGUS_FIX_RATE - NO_ARGUS_FIX_RATE

# ╔═╡ 10d48fb5-d1f1-4cb3-8087-368e42cf8bf6
TIMES_FIX = (WITH_ARGUS_FIX_RATE / NO_ARGUS_FIX_RATE) |> v -> round(v, sigdigits=2)

# ╔═╡ 00deb41f-ddd5-4e8a-8e66-ed640f9e0e15
md"""
- With Argus fix rate

  $WITH_ARGUS_FIX_RATE (CI = $WITH_ARGUS_FIX_CI)

- Without Argus fix rate 
  $NO_ARGUS_FIX_RATE (CI = $NO_ARGUS_FIX_CI)

A difference of $FIX_RATE_PP pp or $TIMES_FIX x.

Using a mixed-effects generalized linear model (to account for within-subjects design), the effect is statistically significant. *p = 0.0262* (Shown below at the intersectino of `has_argus` and `p`.)
"""

# ╔═╡ 0d460887-d910-4b05-aea8-d24f390e4833
time_outcomes_latex(FIX_RATE; prefix="Fix", percents=true) |> println

# ╔═╡ 4376aecc-078e-4c20-ab4f-1bd0843dea07
ChisqTest(freqtable(all_tasks.is_success, all_tasks.has_argus))

# ╔═╡ 522e5282-712f-4d80-a379-08ed015c8610
md"""
---
#### Fix Times
"""

# ╔═╡ 77d5226d-0759-4133-b934-9155c4499b20
KruskalWallisTest(with_argus(all_tasks).durationint, no_argus(all_tasks).durationint)

# ╔═╡ b0aece7e-c7ef-404c-bab4-69b1cf85fb3e
WITH_ARGUS_FIX_MD = with_argus(all_tasks).durationint |> median |> round |> s -> canonicalize(Second(s))

# ╔═╡ 4681a0d4-81e5-42e1-98c4-2b587f438c7b
WITH_ARGUS_FIXED_CI = SignTest(
	with_argus(all_tasks).durationint
) |> confint |> t -> map(s -> canonicalize(Second(s)), t)

# ╔═╡ cafb4dbc-6397-485e-a694-a018359ca594
NO_ARGUS_FIX_MD = no_argus(all_tasks).durationint |> median |> round |> s -> canonicalize(Second(s))

# ╔═╡ 24e8ebf2-43a3-42d7-a4d9-6938e4804caa
NO_ARGUS_FIXED_CI = SignTest(no_argus(all_tasks).durationint) |> confint |> t -> map(s -> canonicalize(Second(s)), t)

# ╔═╡ 6bf1d725-d99c-4871-8c16-cc258839bd04
FIXED_DIFF = NO_ARGUS_FIX_MD - WITH_ARGUS_FIX_MD

# ╔═╡ b001acee-0b60-4904-b62b-c9e5a7b4d74f
TIMES_FIXED = (
	Dates.toms(NO_ARGUS_FIX_MD) / Dates.toms(WITH_ARGUS_FIX_MD)
) |> v -> round(v, sigdigits=2)

# ╔═╡ cda4f21e-44a5-43d7-9052-5d7be025d2cd
md"""
- With Argus fix times 

  $WITH_ARGUS_FIX_MD (CI = $WITH_ARGUS_FIXED_CI)

- Without Argus fix times

  $NO_ARGUS_FIX_MD (CI = $NO_ARGUS_FIXED_CI)

A difference of $FIXED_DIFF pp or $TIMES_FIXED x

Using a Kruskal-Wallis test, this effect is statistically significant *(X(1, 100) = 5.04327 shown under `Details`.)*
"""

# ╔═╡ 79730297-d61f-4bbb-b8ef-934862d3b197
md"""
### RQ2
"""

# ╔═╡ 30994f1e-eaf3-427d-87ab-fc5a7927b0cc
md"""
*Note, these numbers are only available/accurate for version 3 of the study_info.*
"""

# ╔═╡ 85d64f14-476c-408b-8080-8f1782ccc1b4
occurrences = begin
	table = DataFrame(CSV.File("./data/CEL-Trait-Debugging - Occurrences.csv"; 
		types=Dict(
		:EditorSearch => String,
		:UsedTopDown => String,
		:FoundSystemParamWOArgus => String,
		:FoundParamishWOArgus => String,
		:OpenedDocs => String,
		)
	))
	
	count_ones(s; f = c -> c == '1') = count(f, coalesce(s, "----"))
	
	(
		table |> pipe(:EditorSearch => ByRow(count_ones) => :TimesSearched)
		|> pipe(:OpenedDocs => ByRow(count_ones) => :TimesDocs)
		|> pipe(:UsedTopDown => ByRow(count_ones) => :TimesTopDown)
		|> pipe(:FoundParamishWOArgus => ByRow(count_ones) => :TimesParamishFound)
		|> pipe(:FoundParamishWOArgus => ByRow(
			s -> count_ones(s; f = c -> c == '1' || c == '0')
		) => :TimesParamish)
	)
end;

# ╔═╡ 2da5246e-7a7b-4058-9a7c-ca90dc176c9f
SEARCHED_IN_EDITOR = sum(occurrences.TimesSearched) / (N * 4)

# ╔═╡ c43c1615-7e09-432f-b373-d0daf80eb434
OPENED_DOCS = sum(occurrences.TimesDocs) / (N * 4)

# ╔═╡ 63fdefda-00fc-46e8-b85c-30be7d662f42
USED_TOP_DOWN = sum(occurrences.TimesTopDown) / (N * 2)

# ╔═╡ cbc2edab-7d77-4c87-8e57-82a0099fc34e
FOUND_PARAMISH = sum(occurrences.TimesParamishFound) / sum(occurrences.TimesParamish)

# ╔═╡ 2443a333-e6e6-4550-8a62-6c7a546d3380
md"""
## 5.2. Inertia Analysis
"""

# ╔═╡ 65db8d3c-a231-44bc-a5b9-b3d09619f272
md"""
### 5.2.2. Results
"""

# ╔═╡ 56a2e8cb-eee7-4427-a9c2-a53baac7b794
read_precision_data(prefix="data/") = (
		map(
			s -> begin 
				nm = @sprintf("./%sheuristic-precision[%s].csv", prefix, s)
				df = CSV.read(nm, DataFrame)
				df.strategy .= s
				df
			end,
			[:rust, :inertia, :depth, :vars]
		)
		|> dfs -> reduce(vcat, dfs)
		|> mp(:filename, (s -> split(s, "/") |> last))
		|> pipe(Cols(:workspace, :filename) => ByRow((s1, s2) -> s1 * "/" * s2) => :name)
);

# ╔═╡ c13826e8-2230-4d06-90f7-769d95360451
read_performance_data(prefix="data/") = (
	CSV.read(@sprintf("./%sdnf-perf.csv", prefix), DataFrame)
)

# ╔═╡ bff76e5a-c9d5-4051-8ffc-e0ddb7423a5b
precision_results = read_precision_data();

# ╔═╡ 4f6da2c6-ffc3-4684-8b32-1f576fa785c0
dnf_performance_results = read_performance_data();

# ╔═╡ 08cc6777-c9b0-410b-bc55-cceb99015cb9
function strategy_median(s)
	precision_results |> filt(r -> r.strategy == s) |> df -> median(df.rank)
end;

# ╔═╡ f3e6e227-9849-41fd-b99f-3c95868b4ad2
INERTIA_MD = strategy_median(:inertia)

# ╔═╡ f6f691e2-14c3-4483-b822-ee3a746271bc
DEPTH_MD = strategy_median(:depth)

# ╔═╡ 477c8a45-063b-4e96-8a60-db99dee9ec5d
VARS_MD = strategy_median(:vars)

# ╔═╡ 873f7287-5a3c-4253-8979-71e4bb1fe9a0
RUST_MD = strategy_median(:rust)

# ╔═╡ c4eb217b-81d7-4128-8e21-935475f72214
md"""
#### Heuristic “Precision”

- Inertia median distance $INERTIA_MD
- Predicate depth median distance $DEPTH_MD
- Inference vars median distance $VARS_MD
- Rust diagnostic median distance $RUST_MD
"""

# ╔═╡ bcc4a4b9-ee24-4982-bae3-37f39e54a3d1
DNF_TREE_MD = dnf_performance_results.N |> median

# ╔═╡ 1d766cbb-82f5-4f36-b023-258823bd663b
DNF_TREE_MIN = dnf_performance_results.N |> minimum

# ╔═╡ 39a8c949-a9fc-4b91-91c0-f0d772484c50
DNF_TREE_MAX = dnf_performance_results.N |> maximum

# ╔═╡ 8901d2db-09a8-4a5b-bd2e-8ee8428f4cb2
md"""
- Inference tree median size $DNF_TREE_MD
- Inference tree min size $DNF_TREE_MIN
- Inference tree max size $DNF_TREE_MAX
"""

# ╔═╡ 767f035d-fa4b-4099-9e37-cd85e9a0d83f
DNF_NORM_MD = dnf_performance_results.Time |> median |> s -> s * 1000

# ╔═╡ 77d700c2-555a-4793-9920-4e0778722fe7
DNF_NORM_MIN = dnf_performance_results.Time |> minimum |> s -> s * 1000

# ╔═╡ 78cfc659-4925-42d5-9e25-4250cb956d3c
DNF_NORM_MAX = dnf_performance_results.Time |> maximum |> s -> s * 1000

# ╔═╡ 00a88397-7168-47bf-a53d-794577dd490f
md"""
- Normalization median (*ms*) $DNF_NORM_MD
- Minimum $DNF_NORM_MIN
- Maximum $DNF_NORM_MAX

*Note, these times were gathered by running the Argus tool on the test suite. Tests run on a 2023 MacBook Pro M3 Pro laptop.*

Numbers below are in **milliseconds**.
"""

# ╔═╡ 6ba89644-0486-4334-bb1f-1907e69ec927
md"""
### Figure 11
"""

# ╔═╡ 97ef8fea-d22a-4929-a975-a69690cdfac5
function fig11_a(df = precision_results) 

	fig = Figure()

	# Precision Plots
	agdraw!(
		fig[1, 1],
		agdata(df) * 
		mapping(
			:strategy => sorter([:inertia, :depth, :vars, :rust]) => "Heuristic", 
			:rank => "Rank", 
			color=:strategy=>"",
			marker=:strategy
		) * 
		visual(Beeswarm; legend=(; framevisible=false), markersize=12), 
		axis=(
			xgridvisible=false,
			rightspinevisible = false,
        	topspinevisible = false,
		)
	)
	fig
end;

# ╔═╡ 68ad58b0-c6e4-4609-b29a-fe594b37e6f9
function fig11_b(df = dnf_performance_results)	
	draw(
		agdata(df) * 
		mapping(
			:N => "Number of Tree Nodes (N)",
			:Time => (v -> v * 1000) => "Time (ms)",
		) * 
		visual(Scatter),
		axis=(
			xgridvisible=false,
			rightspinevisible = false,
        	topspinevisible = false
		)
	)
end;

# ╔═╡ c44c1a73-3ce3-48ca-9108-4657654405a5
md"""
*Note to reviewers, Figure 11 (a) in the submitted PDF contains only a single yellow triangle for the inertia heuristic. The final paper will contain all data points but grouped tighter.*
"""

# ╔═╡ bfc8ba94-4b00-4440-b3c6-3f322749f6c8
md"""
**These figures use the data included in the artifact submission.** If you previously ran the command `generate-data`, there will be two graphs below for comparison.
"""

# ╔═╡ 9b0bb6c2-7df2-4030-9039-a1e65af46825
FIG11_A = fig11_a()

# ╔═╡ 683328c8-40f1-4b4b-bc99-9ea316fb51be
FIG11_B = fig11_b()

# ╔═╡ 2a194322-6246-4ede-9e41-30fffaa7765b
md"""
#### Figure 11 with data run locally.
"""

# ╔═╡ 5feec8d9-c530-474d-8c42-e1949f439cfb
if isdir("data/gen") 
  fig11_a(read_precision_data("data/gen/"))
else
  md"""
  To view Figure 11 (a) with data generated locally, run the `run-evaluation` command.

  *Nothing to show*
  """
end

# ╔═╡ 289db6e8-79e5-4ef5-945f-ba29b8e0a5d9
if isdir("data/gen") 
  fig11_b(read_performance_data("data/gen/"))
else
  md"""
  To view Figure 11 (b) with data generated locally, run the `run-evaluation` command.

  *Nothing to show*
  """
end

# ╔═╡ 56764702-3b30-47b6-916f-f62ec29b4d16
md"""
---
"""

# ╔═╡ 5c23113e-f136-486c-8735-2aeeb178ade2
"""
Export all of the figures in this notebook to the `./figs` directory.
"""
function export_figures()
	save("./figs/Fig10_a.pdf", FIG10_A, px_per_unit = 2)
	save("./figs/Fig10_b.pdf", FIG10_B, px_per_unit = 2)
	save("./figs/Fig10_c.pdf", FIG10_C, px_per_unit = 2)
	save("./figs/Fig10_d.pdf", FIG10_D, px_per_unit = 2)
	save("./figs/Fig11_a.pdf", FIG11_A, px_per_unit = 2)
	save("./figs/Fig11_b.pdf", FIG11_B, px_per_unit = 2)
end

# ╔═╡ 1071b333-dd21-474c-8378-ae5517fa4e2d
export_figures()

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
AlgebraOfGraphics = "cbdf2221-f076-402e-a563-3d30da359d67"
CairoMakie = "13f3f980-e62b-5c42-98c6-ff1f3baf88f0"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
FreqTables = "da1fdf0e-e0ff-5433-a45f-9bb5ff651cb1"
HypothesisTests = "09f84164-cd44-5f33-b23f-e6b0d136a0d5"
LaTeXStrings = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
MixedModels = "ff71e718-51f3-5ec2-a782-8ffcbfa3c316"
PlutoLinks = "0ff47ea0-7a50-410d-8455-4348d5de0420"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Printf = "de0858da-6303-5e67-8744-51eddeeeb8d7"
StatsKit = "2cb19f9e-ec4d-5c53-8573-a4542a68d3f0"
SwarmMakie = "0b1c068e-6a84-4e66-8136-5c95cafa83ed"
TimeSeries = "9e3dc215-6440-5c97-bce1-76c03772f85e"

[compat]
AlgebraOfGraphics = "~0.8.13"
CairoMakie = "~0.12.14"
DataFrames = "~1.7.0"
FreqTables = "~0.4.6"
HypothesisTests = "~0.11.0"
LaTeXStrings = "~1.4.0"
MixedModels = "~4.25.4"
PlutoLinks = "~0.1.6"
PlutoUI = "~0.7.60"
StatsKit = "~0.3.1"
SwarmMakie = "~0.1.2"
TimeSeries = "~0.24.2"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.11.3"
manifest_format = "2.0"
project_hash = "6de35417bd65def5932d47a9516ca49ea3759e14"

[[deps.AbstractFFTs]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "d92ad398961a3ed262d8bf04a1a2b8340f915fef"
uuid = "621f4979-c628-5d54-868e-fcf4e3e8185c"
version = "1.5.0"
weakdeps = ["ChainRulesCore", "Test"]

    [deps.AbstractFFTs.extensions]
    AbstractFFTsChainRulesCoreExt = "ChainRulesCore"
    AbstractFFTsTestExt = "Test"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "6e1d2a35f2f90a4bc7c2ed98079b2ba09c35b83a"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.3.2"

[[deps.AbstractTrees]]
git-tree-sha1 = "2d9c9a55f9c93e8887ad391fbae72f8ef55e1177"
uuid = "1520ce14-60c1-5f80-bbc7-55ef81b5835c"
version = "0.4.5"

[[deps.Accessors]]
deps = ["CompositionsBase", "ConstructionBase", "Dates", "InverseFunctions", "MacroTools"]
git-tree-sha1 = "3b86719127f50670efe356bc11073d84b4ed7a5d"
uuid = "7d9f7c33-5ae7-4f3b-8dc6-eff91059b697"
version = "0.1.42"

    [deps.Accessors.extensions]
    AxisKeysExt = "AxisKeys"
    IntervalSetsExt = "IntervalSets"
    LinearAlgebraExt = "LinearAlgebra"
    StaticArraysExt = "StaticArrays"
    StructArraysExt = "StructArrays"
    TestExt = "Test"
    UnitfulExt = "Unitful"

    [deps.Accessors.weakdeps]
    AxisKeys = "94b1ba4f-4ee9-5380-92f1-94cde586c3c5"
    IntervalSets = "8197267c-284f-5f27-9208-e0e47529a953"
    LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"
    StructArrays = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
    Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
    Unitful = "1986cc42-f94f-5a68-af5c-568840ba703d"

[[deps.Adapt]]
deps = ["LinearAlgebra", "Requires"]
git-tree-sha1 = "f7817e2e585aa6d924fd714df1e2a84be7896c60"
uuid = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
version = "4.3.0"
weakdeps = ["SparseArrays", "StaticArrays"]

    [deps.Adapt.extensions]
    AdaptSparseArraysExt = "SparseArrays"
    AdaptStaticArraysExt = "StaticArrays"

[[deps.AdaptivePredicates]]
git-tree-sha1 = "7e651ea8d262d2d74ce75fdf47c4d63c07dba7a6"
uuid = "35492f91-a3bd-45ad-95db-fcad7dcfedb7"
version = "1.2.0"

[[deps.AlgebraOfGraphics]]
deps = ["Accessors", "Colors", "Dates", "Dictionaries", "FileIO", "GLM", "GeoInterface", "GeometryBasics", "GridLayoutBase", "Isoband", "KernelDensity", "Loess", "Makie", "NaturalSort", "PlotUtils", "PolygonOps", "PooledArrays", "PrecompileTools", "RelocatableFolders", "StatsBase", "StructArrays", "Tables"]
git-tree-sha1 = "ad7d27bb258200fde0f8e9a7df5802dc5cda1d26"
uuid = "cbdf2221-f076-402e-a563-3d30da359d67"
version = "0.8.14"

[[deps.AliasTables]]
deps = ["PtrArrays", "Random"]
git-tree-sha1 = "9876e1e164b144ca45e9e3198d0b689cadfed9ff"
uuid = "66dad0bd-aa9a-41b7-9441-69ab47430ed8"
version = "1.1.3"

[[deps.Animations]]
deps = ["Colors"]
git-tree-sha1 = "e092fa223bf66a3c41f9c022bd074d916dc303e7"
uuid = "27a7e980-b3e6-11e9-2bcd-0b925532e340"
version = "0.4.2"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.2"

[[deps.Arpack]]
deps = ["Arpack_jll", "Libdl", "LinearAlgebra", "Logging"]
git-tree-sha1 = "9b9b347613394885fd1c8c7729bfc60528faa436"
uuid = "7d9fca2a-8960-54d3-9f78-7d1dccf2cb97"
version = "0.5.4"

[[deps.Arpack_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "OpenBLAS_jll", "Pkg"]
git-tree-sha1 = "5ba6c757e8feccf03a1554dfaf3e26b3cfc7fd5e"
uuid = "68821587-b530-5797-8361-c406ea357684"
version = "3.5.1+1"

[[deps.ArrayLayouts]]
deps = ["FillArrays", "LinearAlgebra"]
git-tree-sha1 = "4e25216b8fea1908a0ce0f5d87368587899f75be"
uuid = "4c555306-a7a7-4459-81d9-ec55ddd5c99a"
version = "1.11.1"
weakdeps = ["SparseArrays"]

    [deps.ArrayLayouts.extensions]
    ArrayLayoutsSparseArraysExt = "SparseArrays"

[[deps.Arrow]]
deps = ["ArrowTypes", "BitIntegers", "CodecLz4", "CodecZstd", "ConcurrentUtilities", "DataAPI", "Dates", "EnumX", "Mmap", "PooledArrays", "SentinelArrays", "StringViews", "Tables", "TimeZones", "TranscodingStreams", "UUIDs"]
git-tree-sha1 = "00f0b3f05bc33cc5b68db6cc22e4a7b16b65e505"
uuid = "69666777-d1a9-59fb-9406-91d4454c9d45"
version = "2.8.0"

[[deps.ArrowTypes]]
deps = ["Sockets", "UUIDs"]
git-tree-sha1 = "404265cd8128a2515a81d5eae16de90fdef05101"
uuid = "31f734f8-188a-4ce0-8406-c8a06bd891cd"
version = "2.3.0"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"
version = "1.11.0"

[[deps.Automa]]
deps = ["PrecompileTools", "SIMD", "TranscodingStreams"]
git-tree-sha1 = "a8f503e8e1a5f583fbef15a8440c8c7e32185df2"
uuid = "67c07d97-cdcb-5c2c-af73-a7f9c32a568b"
version = "1.1.0"

[[deps.AxisAlgorithms]]
deps = ["LinearAlgebra", "Random", "SparseArrays", "WoodburyMatrices"]
git-tree-sha1 = "01b8ccb13d68535d73d2b0c23e39bd23155fb712"
uuid = "13072b0f-2c55-5437-9ae7-d433b7a33950"
version = "1.1.0"

[[deps.AxisArrays]]
deps = ["Dates", "IntervalSets", "IterTools", "RangeArrays"]
git-tree-sha1 = "16351be62963a67ac4083f748fdb3cca58bfd52f"
uuid = "39de3d68-74b9-583c-8d2d-e117c070f3a9"
version = "0.4.7"

[[deps.BSplineKit]]
deps = ["ArrayLayouts", "BandedMatrices", "FastGaussQuadrature", "ForwardDiff", "LinearAlgebra", "PrecompileTools", "Random", "Reexport", "SparseArrays", "Static", "StaticArrays", "StaticArraysCore"]
git-tree-sha1 = "15ab25b14c48783b1b73f80b14883fce7050daea"
uuid = "093aae92-e908-43d7-9660-e50ee39d5a0a"
version = "0.17.7"

[[deps.BandedMatrices]]
deps = ["ArrayLayouts", "FillArrays", "LinearAlgebra", "PrecompileTools"]
git-tree-sha1 = "614c6aba1d562191d9832df2af24f594aa7ebf61"
uuid = "aae01518-5342-5314-be14-df237901396f"
version = "1.9.3"
weakdeps = ["SparseArrays"]

    [deps.BandedMatrices.extensions]
    BandedMatricesSparseArraysExt = "SparseArrays"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"
version = "1.11.0"

[[deps.BitIntegers]]
deps = ["Random"]
git-tree-sha1 = "6158239ac409f960abbc232a9b24c00f5cce3108"
uuid = "c3b6d118-76ef-56ca-8cc7-ebb389d030a1"
version = "0.3.2"

[[deps.Bootstrap]]
deps = ["DataFrames", "Distributions", "Random", "Statistics", "StatsBase", "StatsModels"]
git-tree-sha1 = "b605e84e7236671cae810accaf20b7678849ac6a"
uuid = "e28b5b4c-05e8-5b66-bc03-6f0c0a0a06e0"
version = "2.4.0"

[[deps.Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1b96ea4a01afe0ea4090c5c8039690672dd13f2e"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.9+0"

[[deps.CEnum]]
git-tree-sha1 = "389ad5c84de1ae7cf0e28e381131c98ea87d54fc"
uuid = "fa961155-64e5-5f13-b03f-caf6b980ea82"
version = "0.5.0"

[[deps.CRC32c]]
uuid = "8bf52ea8-c179-5cab-976a-9e18b702a9bc"
version = "1.11.0"

[[deps.CRlibm_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e329286945d0cfc04456972ea732551869af1cfc"
uuid = "4e9b3aee-d8a1-5a3d-ad8b-7d824db253f0"
version = "1.0.1+0"

[[deps.CSV]]
deps = ["CodecZlib", "Dates", "FilePathsBase", "InlineStrings", "Mmap", "Parsers", "PooledArrays", "PrecompileTools", "SentinelArrays", "Tables", "Unicode", "WeakRefStrings", "WorkerUtilities"]
git-tree-sha1 = "deddd8725e5e1cc49ee205a1964256043720a6c3"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.10.15"

[[deps.Cairo]]
deps = ["Cairo_jll", "Colors", "Glib_jll", "Graphics", "Libdl", "Pango_jll"]
git-tree-sha1 = "71aa551c5c33f1a4415867fe06b7844faadb0ae9"
uuid = "159f3aea-2a34-519c-b102-8c37f9878175"
version = "1.1.1"

[[deps.CairoMakie]]
deps = ["CRC32c", "Cairo", "Cairo_jll", "Colors", "FileIO", "FreeType", "GeometryBasics", "LinearAlgebra", "Makie", "PrecompileTools"]
git-tree-sha1 = "0afa2b4ac444b9412130d68493941e1af462e26a"
uuid = "13f3f980-e62b-5c42-98c6-ff1f3baf88f0"
version = "0.12.18"

[[deps.Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "CompilerSupportLibraries_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "009060c9a6168704143100f36ab08f06c2af4642"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.18.2+1"

[[deps.CategoricalArrays]]
deps = ["DataAPI", "Future", "Missings", "Printf", "Requires", "Statistics", "Unicode"]
git-tree-sha1 = "1568b28f91293458345dabba6a5ea3f183250a61"
uuid = "324d7699-5711-5eae-9e2f-1d82baa6b597"
version = "0.10.8"
weakdeps = ["JSON", "RecipesBase", "SentinelArrays", "StructTypes"]

    [deps.CategoricalArrays.extensions]
    CategoricalArraysJSONExt = "JSON"
    CategoricalArraysRecipesBaseExt = "RecipesBase"
    CategoricalArraysSentinelArraysExt = "SentinelArrays"
    CategoricalArraysStructTypesExt = "StructTypes"

[[deps.ChainRulesCore]]
deps = ["Compat", "LinearAlgebra"]
git-tree-sha1 = "1713c74e00545bfe14605d2a2be1712de8fbcb58"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.25.1"
weakdeps = ["SparseArrays"]

    [deps.ChainRulesCore.extensions]
    ChainRulesCoreSparseArraysExt = "SparseArrays"

[[deps.Clustering]]
deps = ["Distances", "LinearAlgebra", "NearestNeighbors", "Printf", "Random", "SparseArrays", "Statistics", "StatsBase"]
git-tree-sha1 = "3e22db924e2945282e70c33b75d4dde8bfa44c94"
uuid = "aaaa29a8-35af-508c-8bc3-b662a17a0fe5"
version = "0.15.8"

[[deps.CodeTracking]]
deps = ["InteractiveUtils", "UUIDs"]
git-tree-sha1 = "7eee164f122511d3e4e1ebadb7956939ea7e1c77"
uuid = "da1fd8a2-8d9e-5ec2-8556-3022fb5608a2"
version = "1.3.6"

[[deps.CodecLz4]]
deps = ["Lz4_jll", "TranscodingStreams"]
git-tree-sha1 = "0db0c70ca94c0a79cadad269497f25ca88b9fa91"
uuid = "5ba52731-8f18-5e0d-9241-30f10d1ec561"
version = "0.4.5"

[[deps.CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "962834c22b66e32aa10f7611c08c8ca4e20749a9"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.8"

[[deps.CodecZstd]]
deps = ["TranscodingStreams", "Zstd_jll"]
git-tree-sha1 = "d0073f473757f0d39ac9707f1eb03b431573cbd8"
uuid = "6b39b394-51ab-5f42-8807-6242bab2b4c2"
version = "0.8.6"

[[deps.ColorBrewer]]
deps = ["Colors", "JSON"]
git-tree-sha1 = "e771a63cc8b539eca78c85b0cabd9233d6c8f06f"
uuid = "a2cac450-b92f-5266-8821-25eda20663c8"
version = "0.4.1"

[[deps.ColorSchemes]]
deps = ["ColorTypes", "ColorVectorSpace", "Colors", "FixedPointNumbers", "PrecompileTools", "Random"]
git-tree-sha1 = "403f2d8e209681fcbd9468a8514efff3ea08452e"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.29.0"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "b10d0b65641d57b8b4d5e234446582de5047050d"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.5"

[[deps.ColorVectorSpace]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "Requires", "Statistics", "TensorCore"]
git-tree-sha1 = "a1f44953f2382ebb937d60dafbe2deea4bd23249"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.10.0"
weakdeps = ["SpecialFunctions"]

    [deps.ColorVectorSpace.extensions]
    SpecialFunctionsExt = "SpecialFunctions"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "64e15186f0aa277e174aa81798f7eb8598e0157e"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.13.0"

[[deps.Combinatorics]]
git-tree-sha1 = "08c8b6831dc00bfea825826be0bc8336fc369860"
uuid = "861a8166-3701-5b0c-9a16-15d98fcdc6aa"
version = "1.0.2"

[[deps.CommonSolve]]
git-tree-sha1 = "0eee5eb66b1cf62cd6ad1b460238e60e4b09400c"
uuid = "38540f10-b2f7-11e9-35d8-d573e4eb0ff2"
version = "0.2.4"

[[deps.CommonSubexpressions]]
deps = ["MacroTools"]
git-tree-sha1 = "cda2cfaebb4be89c9084adaca7dd7333369715c5"
uuid = "bbf7d656-a473-5ed7-a52c-81e309532950"
version = "0.3.1"

[[deps.CommonWorldInvalidations]]
git-tree-sha1 = "ae52d1c52048455e85a387fbee9be553ec2b68d0"
uuid = "f70d9fcc-98c5-4d4a-abd7-e4cdeebd8ca8"
version = "1.0.0"

[[deps.Compat]]
deps = ["TOML", "UUIDs"]
git-tree-sha1 = "8ae8d32e09f0dcf42a36b90d4e17f5dd2e4c4215"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "4.16.0"
weakdeps = ["Dates", "LinearAlgebra"]

    [deps.Compat.extensions]
    CompatLinearAlgebraExt = "LinearAlgebra"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.1.1+0"

[[deps.CompositionsBase]]
git-tree-sha1 = "802bb88cd69dfd1509f6670416bd4434015693ad"
uuid = "a33af91c-f02d-484b-be07-31d278c5ca2b"
version = "0.1.2"
weakdeps = ["InverseFunctions"]

    [deps.CompositionsBase.extensions]
    CompositionsBaseInverseFunctionsExt = "InverseFunctions"

[[deps.ConcurrentUtilities]]
deps = ["Serialization", "Sockets"]
git-tree-sha1 = "d9d26935a0bcffc87d2613ce14c527c99fc543fd"
uuid = "f0e56b4a-5159-44fe-b623-3e5288b988bb"
version = "2.5.0"

[[deps.ConstructionBase]]
git-tree-sha1 = "76219f1ed5771adbb096743bff43fb5fdd4c1157"
uuid = "187b0558-2788-49d3-abe0-74a17ed4e7c9"
version = "1.5.8"
weakdeps = ["IntervalSets", "LinearAlgebra", "StaticArrays"]

    [deps.ConstructionBase.extensions]
    ConstructionBaseIntervalSetsExt = "IntervalSets"
    ConstructionBaseLinearAlgebraExt = "LinearAlgebra"
    ConstructionBaseStaticArraysExt = "StaticArrays"

[[deps.Contour]]
git-tree-sha1 = "439e35b0b36e2e5881738abc8857bd92ad6ff9a8"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.6.3"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "abe83f3a2f1b857aac70ef8b269080af17764bbe"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.16.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "DataStructures", "Future", "InlineStrings", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrecompileTools", "PrettyTables", "Printf", "Random", "Reexport", "SentinelArrays", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "fb61b4812c49343d7ef0b533ba982c46021938a6"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.7.0"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "1d0a14036acb104d9e89698bd408f63ab58cdc82"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.20"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
version = "1.11.0"

[[deps.DelaunayTriangulation]]
deps = ["AdaptivePredicates", "EnumX", "ExactPredicates", "Random"]
git-tree-sha1 = "5620ff4ee0084a6ab7097a27ba0c19290200b037"
uuid = "927a84f5-c5f4-47a5-9785-b46e178433df"
version = "1.6.4"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
git-tree-sha1 = "9e2f36d3c96a820c678f2f1f1782582fcf685bae"
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"
version = "1.9.1"

[[deps.Dictionaries]]
deps = ["Indexing", "Random", "Serialization"]
git-tree-sha1 = "1cdab237b6e0d0960d5dcbd2c0ebfa15fa6573d9"
uuid = "85a47980-9c8c-11e8-2b9f-f7ca1fa99fb4"
version = "0.4.4"

[[deps.DiffResults]]
deps = ["StaticArraysCore"]
git-tree-sha1 = "782dd5f4561f5d267313f23853baaaa4c52ea621"
uuid = "163ba53b-c6d8-5494-b064-1a9d43ac40c5"
version = "1.1.0"

[[deps.DiffRules]]
deps = ["IrrationalConstants", "LogExpFunctions", "NaNMath", "Random", "SpecialFunctions"]
git-tree-sha1 = "23163d55f885173722d1e4cf0f6110cdbaf7e272"
uuid = "b552c78f-8df3-52c6-915a-8e097449b14b"
version = "1.15.1"

[[deps.Distances]]
deps = ["LinearAlgebra", "Statistics", "StatsAPI"]
git-tree-sha1 = "c7e3a542b999843086e2f29dac96a618c105be1d"
uuid = "b4f34e82-e78d-54a5-968a-f98e89d6e8f7"
version = "0.10.12"
weakdeps = ["ChainRulesCore", "SparseArrays"]

    [deps.Distances.extensions]
    DistancesChainRulesCoreExt = "ChainRulesCore"
    DistancesSparseArraysExt = "SparseArrays"

[[deps.Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"
version = "1.11.0"

[[deps.Distributions]]
deps = ["AliasTables", "FillArrays", "LinearAlgebra", "PDMats", "Printf", "QuadGK", "Random", "SpecialFunctions", "Statistics", "StatsAPI", "StatsBase", "StatsFuns"]
git-tree-sha1 = "0b4190661e8a4e51a842070e7dd4fae440ddb7f4"
uuid = "31c24e10-a181-5473-b8eb-7969acd0382f"
version = "0.25.118"

    [deps.Distributions.extensions]
    DistributionsChainRulesCoreExt = "ChainRulesCore"
    DistributionsDensityInterfaceExt = "DensityInterface"
    DistributionsTestExt = "Test"

    [deps.Distributions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    DensityInterface = "b429d917-457f-4dbc-8f4c-0cc954292b1d"
    Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "2fb1e02f2b635d0845df5d7c167fec4dd739b00d"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.9.3"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.EarCut_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e3290f2d49e661fbd94046d7e3726ffcb2d41053"
uuid = "5ae413db-bbd1-5e63-b57d-d24a61df00f5"
version = "2.2.4+0"

[[deps.EnumX]]
git-tree-sha1 = "bdb1942cd4c45e3c678fd11569d5cccd80976237"
uuid = "4e289a0a-7415-4d19-859d-a7e5c4648b56"
version = "1.0.4"

[[deps.ExactPredicates]]
deps = ["IntervalArithmetic", "Random", "StaticArrays"]
git-tree-sha1 = "b3f2ff58735b5f024c392fde763f29b057e4b025"
uuid = "429591f6-91af-11e9-00e2-59fbe8cec110"
version = "2.2.8"

[[deps.Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "d55dffd9ae73ff72f1c0482454dcf2ec6c6c4a63"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.6.5+0"

[[deps.ExprTools]]
git-tree-sha1 = "27415f162e6028e81c72b82ef756bf321213b6ec"
uuid = "e2ba6199-217a-4e67-a87a-7c52f15ade04"
version = "0.1.10"

[[deps.Extents]]
git-tree-sha1 = "063512a13dbe9c40d999c439268539aa552d1ae6"
uuid = "411431e0-e8b7-467b-b5e0-f676ba4f2910"
version = "0.1.5"

[[deps.FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "PCRE2_jll", "Zlib_jll", "libaom_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "8cc47f299902e13f90405ddb5bf87e5d474c0d38"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "6.1.2+0"

[[deps.FFTW]]
deps = ["AbstractFFTs", "FFTW_jll", "LinearAlgebra", "MKL_jll", "Preferences", "Reexport"]
git-tree-sha1 = "7de7c78d681078f027389e067864a8d53bd7c3c9"
uuid = "7a1cc6ca-52ef-59f5-83cd-3a7055c09341"
version = "1.8.1"

[[deps.FFTW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4d81ed14783ec49ce9f2e168208a12ce1815aa25"
uuid = "f5851436-0d7a-5f13-b9de-f02708fd171a"
version = "3.3.10+3"

[[deps.FastGaussQuadrature]]
deps = ["LinearAlgebra", "SpecialFunctions", "StaticArrays"]
git-tree-sha1 = "fd923962364b645f3719855c88f7074413a6ad92"
uuid = "442a2c76-b920-505d-bb47-c5924d526838"
version = "1.0.2"

[[deps.FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "b66970a70db13f45b7e57fbda1736e1cf72174ea"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.17.0"

    [deps.FileIO.extensions]
    HTTPExt = "HTTP"

    [deps.FileIO.weakdeps]
    HTTP = "cd3eb016-35fb-5094-929b-558a96fad6f3"

[[deps.FilePaths]]
deps = ["FilePathsBase", "MacroTools", "Reexport", "Requires"]
git-tree-sha1 = "919d9412dbf53a2e6fe74af62a73ceed0bce0629"
uuid = "8fc22ac5-c921-52a6-82fd-178b2807b824"
version = "0.8.3"

[[deps.FilePathsBase]]
deps = ["Compat", "Dates"]
git-tree-sha1 = "3bab2c5aa25e7840a4b065805c0cdfc01f3068d2"
uuid = "48062228-2e41-5def-b9a4-89aafe57970f"
version = "0.9.24"
weakdeps = ["Mmap", "Test"]

    [deps.FilePathsBase.extensions]
    FilePathsBaseMmapExt = "Mmap"
    FilePathsBaseTestExt = "Test"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"
version = "1.11.0"

[[deps.FillArrays]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "6a70198746448456524cb442b8af316927ff3e1a"
uuid = "1a297f60-69ca-5386-bcde-b61e274b549b"
version = "1.13.0"
weakdeps = ["PDMats", "SparseArrays", "Statistics"]

    [deps.FillArrays.extensions]
    FillArraysPDMatsExt = "PDMats"
    FillArraysSparseArraysExt = "SparseArrays"
    FillArraysStatisticsExt = "Statistics"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "05882d6995ae5c12bb5f36dd2ed3f61c98cbb172"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.5"

[[deps.Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Zlib_jll"]
git-tree-sha1 = "21fac3c77d7b5a9fc03b0ec503aa1a6392c34d2b"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.15.0+0"

[[deps.Format]]
git-tree-sha1 = "9c68794ef81b08086aeb32eeaf33531668d5f5fc"
uuid = "1fa38f19-a742-5d3f-a2b9-30dd87b9d5f8"
version = "1.3.7"

[[deps.ForwardDiff]]
deps = ["CommonSubexpressions", "DiffResults", "DiffRules", "LinearAlgebra", "LogExpFunctions", "NaNMath", "Preferences", "Printf", "Random", "SpecialFunctions"]
git-tree-sha1 = "a2df1b776752e3f344e5116c06d75a10436ab853"
uuid = "f6369f11-7733-5829-9624-2563aa707210"
version = "0.10.38"
weakdeps = ["StaticArrays"]

    [deps.ForwardDiff.extensions]
    ForwardDiffStaticArraysExt = "StaticArrays"

[[deps.FreeType]]
deps = ["CEnum", "FreeType2_jll"]
git-tree-sha1 = "907369da0f8e80728ab49c1c7e09327bf0d6d999"
uuid = "b38be410-82b0-50bf-ab77-7b57e271db43"
version = "4.1.1"

[[deps.FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "786e968a8d2fb167f2e4880baba62e0e26bd8e4e"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.13.3+1"

[[deps.FreeTypeAbstraction]]
deps = ["ColorVectorSpace", "Colors", "FreeType", "GeometryBasics"]
git-tree-sha1 = "d52e255138ac21be31fa633200b65e4e71d26802"
uuid = "663a7486-cb36-511b-a19d-713bb74d65c9"
version = "0.10.6"

[[deps.FreqTables]]
deps = ["CategoricalArrays", "Missings", "NamedArrays", "Tables"]
git-tree-sha1 = "4693424929b4ec7ad703d68912a6ad6eff103cfe"
uuid = "da1fdf0e-e0ff-5433-a45f-9bb5ff651cb1"
version = "0.4.6"

[[deps.FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "846f7026a9decf3679419122b49f8a1fdb48d2d5"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.16+0"

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"
version = "1.11.0"

[[deps.GLM]]
deps = ["Distributions", "LinearAlgebra", "Printf", "Reexport", "SparseArrays", "SpecialFunctions", "Statistics", "StatsAPI", "StatsBase", "StatsFuns", "StatsModels"]
git-tree-sha1 = "273bd1cd30768a2fddfa3fd63bbc746ed7249e5f"
uuid = "38e38edf-8417-5370-95a0-9cbb8c7f171a"
version = "1.9.0"

[[deps.GeoFormatTypes]]
git-tree-sha1 = "8e233d5167e63d708d41f87597433f59a0f213fe"
uuid = "68eda718-8dee-11e9-39e7-89f7f65f511f"
version = "0.4.4"

[[deps.GeoInterface]]
deps = ["DataAPI", "Extents", "GeoFormatTypes"]
git-tree-sha1 = "294e99f19869d0b0cb71aef92f19d03649d028d5"
uuid = "cf35fbd7-0cd7-5166-be24-54bfbe79505f"
version = "1.4.1"

[[deps.GeometryBasics]]
deps = ["EarCut_jll", "Extents", "GeoInterface", "IterTools", "LinearAlgebra", "StaticArrays", "StructArrays", "Tables"]
git-tree-sha1 = "b62f2b2d76cee0d61a2ef2b3118cd2a3215d3134"
uuid = "5c1252a2-5f33-56bf-86c9-59e7332b4326"
version = "0.4.11"

[[deps.Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[deps.Giflib_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6570366d757b50fabae9f4315ad74d2e40c0560a"
uuid = "59f7168a-df46-5410-90c8-f2779963d0ec"
version = "5.2.3+0"

[[deps.Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE2_jll", "Zlib_jll"]
git-tree-sha1 = "b0036b392358c80d2d2124746c2bf3d48d457938"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.82.4+0"

[[deps.Graphics]]
deps = ["Colors", "LinearAlgebra", "NaNMath"]
git-tree-sha1 = "a641238db938fff9b2f60d08ed9030387daf428c"
uuid = "a2bd30eb-e257-5431-a919-1863eab51364"
version = "1.1.3"

[[deps.Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "01979f9b37367603e2848ea225918a3b3861b606"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+1"

[[deps.GridLayoutBase]]
deps = ["GeometryBasics", "InteractiveUtils", "Observables"]
git-tree-sha1 = "dc6bed05c15523624909b3953686c5f5ffa10adc"
uuid = "3955a311-db13-416c-9275-1d80ed98e5e9"
version = "0.11.1"

[[deps.Grisu]]
git-tree-sha1 = "53bb909d1151e57e2484c3d1b53e19552b887fb2"
uuid = "42e2da0e-8278-4e71-bc24-59509adca0fe"
version = "1.0.2"

[[deps.HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll"]
git-tree-sha1 = "55c53be97790242c29031e5cd45e8ac296dadda3"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "8.5.0+0"

[[deps.HypergeometricFunctions]]
deps = ["LinearAlgebra", "OpenLibm_jll", "SpecialFunctions"]
git-tree-sha1 = "68c173f4f449de5b438ee67ed0c9c748dc31a2ec"
uuid = "34004b35-14d8-5ef3-9330-4cdb6864b03a"
version = "0.3.28"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "179267cfa5e712760cd43dcae385d7ea90cc25a4"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.5"

[[deps.HypertextLiteral]]
deps = ["Tricks"]
git-tree-sha1 = "7134810b1afce04bbc1045ca1985fbe81ce17653"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.5"

[[deps.HypothesisTests]]
deps = ["Combinatorics", "Distributions", "LinearAlgebra", "Printf", "Random", "Rmath", "Roots", "Statistics", "StatsAPI", "StatsBase"]
git-tree-sha1 = "6c3ce99fdbaf680aa6716f4b919c19e902d67c9c"
uuid = "09f84164-cd44-5f33-b23f-e6b0d136a0d5"
version = "0.11.3"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "b6d6bfdd7ce25b0f9b2f6b3dd56b2673a66c8770"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.5"

[[deps.IfElse]]
git-tree-sha1 = "debdd00ffef04665ccbb3e150747a77560e8fad1"
uuid = "615f187c-cbe4-4ef1-ba3b-2fcf58d6d173"
version = "0.1.1"

[[deps.ImageAxes]]
deps = ["AxisArrays", "ImageBase", "ImageCore", "Reexport", "SimpleTraits"]
git-tree-sha1 = "e12629406c6c4442539436581041d372d69c55ba"
uuid = "2803e5a7-5153-5ecf-9a86-9b4c37f5f5ac"
version = "0.6.12"

[[deps.ImageBase]]
deps = ["ImageCore", "Reexport"]
git-tree-sha1 = "eb49b82c172811fd2c86759fa0553a2221feb909"
uuid = "c817782e-172a-44cc-b673-b171935fbb9e"
version = "0.1.7"

[[deps.ImageCore]]
deps = ["ColorVectorSpace", "Colors", "FixedPointNumbers", "MappedArrays", "MosaicViews", "OffsetArrays", "PaddedViews", "PrecompileTools", "Reexport"]
git-tree-sha1 = "8c193230235bbcee22c8066b0374f63b5683c2d3"
uuid = "a09fc81d-aa75-5fe9-8630-4744c3626534"
version = "0.10.5"

[[deps.ImageIO]]
deps = ["FileIO", "IndirectArrays", "JpegTurbo", "LazyModules", "Netpbm", "OpenEXR", "PNGFiles", "QOI", "Sixel", "TiffImages", "UUIDs", "WebP"]
git-tree-sha1 = "696144904b76e1ca433b886b4e7edd067d76cbf7"
uuid = "82e4d734-157c-48bb-816b-45c225c6df19"
version = "0.6.9"

[[deps.ImageMetadata]]
deps = ["AxisArrays", "ImageAxes", "ImageBase", "ImageCore"]
git-tree-sha1 = "2a81c3897be6fbcde0802a0ebe6796d0562f63ec"
uuid = "bc367c6b-8a6b-528e-b4bd-a4b897500b49"
version = "0.9.10"

[[deps.Imath_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "0936ba688c6d201805a83da835b55c61a180db52"
uuid = "905a6f67-0a94-5f89-b386-d35d92009cd1"
version = "3.1.11+0"

[[deps.Indexing]]
git-tree-sha1 = "ce1566720fd6b19ff3411404d4b977acd4814f9f"
uuid = "313cdc1a-70c2-5d6a-ae34-0150d3930a38"
version = "1.1.1"

[[deps.IndirectArrays]]
git-tree-sha1 = "012e604e1c7458645cb8b436f8fba789a51b257f"
uuid = "9b13fd28-a010-5f03-acff-a1bbcff69959"
version = "1.0.0"

[[deps.Inflate]]
git-tree-sha1 = "d1b1b796e47d94588b3757fe84fbf65a5ec4a80d"
uuid = "d25df0c9-e2be-5dd7-82c8-3ad0b3e990b9"
version = "0.1.5"

[[deps.InlineStrings]]
git-tree-sha1 = "6a9fde685a7ac1eb3495f8e812c5a7c3711c2d5e"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.4.3"
weakdeps = ["ArrowTypes", "Parsers"]

    [deps.InlineStrings.extensions]
    ArrowTypesExt = "ArrowTypes"
    ParsersExt = "Parsers"

[[deps.IntelOpenMP_jll]]
deps = ["Artifacts", "JLLWrappers", "LazyArtifacts", "Libdl"]
git-tree-sha1 = "0f14a5456bdc6b9731a5682f439a672750a09e48"
uuid = "1d5cc7b8-4909-519e-a0f8-d0f5ad9712d0"
version = "2025.0.4+0"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
version = "1.11.0"

[[deps.Interpolations]]
deps = ["Adapt", "AxisAlgorithms", "ChainRulesCore", "LinearAlgebra", "OffsetArrays", "Random", "Ratios", "Requires", "SharedArrays", "SparseArrays", "StaticArrays", "WoodburyMatrices"]
git-tree-sha1 = "88a101217d7cb38a7b481ccd50d21876e1d1b0e0"
uuid = "a98d9a8b-a2ab-59e6-89dd-64a1c18fca59"
version = "0.15.1"
weakdeps = ["Unitful"]

    [deps.Interpolations.extensions]
    InterpolationsUnitfulExt = "Unitful"

[[deps.IntervalArithmetic]]
deps = ["CRlibm_jll", "LinearAlgebra", "MacroTools", "OpenBLASConsistentFPCSR_jll", "RoundingEmulator"]
git-tree-sha1 = "7b3603d3a5c52bcb18de8e46fa62e4176055f31e"
uuid = "d1acc4aa-44c8-5952-acd4-ba5d80a2a253"
version = "0.22.25"
weakdeps = ["DiffRules", "ForwardDiff", "IntervalSets", "RecipesBase"]

    [deps.IntervalArithmetic.extensions]
    IntervalArithmeticDiffRulesExt = "DiffRules"
    IntervalArithmeticForwardDiffExt = "ForwardDiff"
    IntervalArithmeticIntervalSetsExt = "IntervalSets"
    IntervalArithmeticRecipesBaseExt = "RecipesBase"

[[deps.IntervalSets]]
git-tree-sha1 = "dba9ddf07f77f60450fe5d2e2beb9854d9a49bd0"
uuid = "8197267c-284f-5f27-9208-e0e47529a953"
version = "0.7.10"
weakdeps = ["Random", "RecipesBase", "Statistics"]

    [deps.IntervalSets.extensions]
    IntervalSetsRandomExt = "Random"
    IntervalSetsRecipesBaseExt = "RecipesBase"
    IntervalSetsStatisticsExt = "Statistics"

[[deps.InverseFunctions]]
git-tree-sha1 = "a779299d77cd080bf77b97535acecd73e1c5e5cb"
uuid = "3587e190-3f89-42d0-90ee-14403ec27112"
version = "0.1.17"
weakdeps = ["Dates", "Test"]

    [deps.InverseFunctions.extensions]
    InverseFunctionsDatesExt = "Dates"
    InverseFunctionsTestExt = "Test"

[[deps.InvertedIndices]]
git-tree-sha1 = "6da3c4316095de0f5ee2ebd875df8721e7e0bdbe"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.3.1"

[[deps.IrrationalConstants]]
git-tree-sha1 = "e2222959fbc6c19554dc15174c81bf7bf3aa691c"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.2.4"

[[deps.Isoband]]
deps = ["isoband_jll"]
git-tree-sha1 = "f9b6d97355599074dc867318950adaa6f9946137"
uuid = "f1662d9f-8043-43de-a69a-05efc1cc6ff4"
version = "0.1.1"

[[deps.IterTools]]
git-tree-sha1 = "42d5f897009e7ff2cf88db414a389e5ed1bdd023"
uuid = "c8e1da08-722c-5040-9ed9-7db0dc04731e"
version = "1.10.0"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLLWrappers]]
deps = ["Artifacts", "Preferences"]
git-tree-sha1 = "a007feb38b422fbdab534406aeca1b86823cb4d6"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.7.0"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "31e996f0a15c7b280ba9f76636b3ff9e2ae58c9a"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.4"

[[deps.JSON3]]
deps = ["Dates", "Mmap", "Parsers", "PrecompileTools", "StructTypes", "UUIDs"]
git-tree-sha1 = "1d322381ef7b087548321d3f878cb4c9bd8f8f9b"
uuid = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"
version = "1.14.1"
weakdeps = ["ArrowTypes"]

    [deps.JSON3.extensions]
    JSON3ArrowExt = ["ArrowTypes"]

[[deps.JpegTurbo]]
deps = ["CEnum", "FileIO", "ImageCore", "JpegTurbo_jll", "TOML"]
git-tree-sha1 = "fa6d0bcff8583bac20f1ffa708c3913ca605c611"
uuid = "b835a17e-a41a-41e7-81f0-2f016b05efe0"
version = "0.1.5"

[[deps.JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "eac1206917768cb54957c65a615460d87b455fc1"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "3.1.1+0"

[[deps.JuliaInterpreter]]
deps = ["CodeTracking", "InteractiveUtils", "Random", "UUIDs"]
git-tree-sha1 = "a434e811d10e7cbf4f0674285542e697dca605d0"
uuid = "aa1ae85d-cabe-5617-a682-6adf51b2e16a"
version = "0.9.42"

[[deps.KernelDensity]]
deps = ["Distributions", "DocStringExtensions", "FFTW", "Interpolations", "StatsBase"]
git-tree-sha1 = "7d703202e65efa1369de1279c162b915e245eed1"
uuid = "5ab0869b-81aa-558d-bb23-cbf5423bbe9b"
version = "0.6.9"

[[deps.LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "170b660facf5df5de098d866564877e119141cbd"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.2+0"

[[deps.LERC_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "aaafe88dccbd957a8d82f7d05be9b69172e0cee3"
uuid = "88015f11-f218-50d7-93a8-a6af411a945d"
version = "4.0.1+0"

[[deps.LLVMOpenMP_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "78211fb6cbc872f77cad3fc0b6cf647d923f4929"
uuid = "1d63c593-3942-5779-bab2-d838dc0a180e"
version = "18.1.7+0"

[[deps.LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1c602b1127f4751facb671441ca72715cc95938a"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.3+0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "dda21b8cbd6a6c40d9d02a73230f9d70fed6918c"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.4.0"

[[deps.LazyArtifacts]]
deps = ["Artifacts", "Pkg"]
uuid = "4af54fe1-eca0-43a8-85a7-787d91b784e3"
version = "1.11.0"

[[deps.LazyModules]]
git-tree-sha1 = "a560dd966b386ac9ae60bdd3a3d3a326062d3c3e"
uuid = "8cdb02fc-e678-4876-92c5-9defec4f444e"
version = "0.3.1"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"
version = "0.6.4"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"
version = "8.6.0+0"

[[deps.LibGit2]]
deps = ["Base64", "LibGit2_jll", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"
version = "1.11.0"

[[deps.LibGit2_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll"]
uuid = "e37daf67-58a4-590a-8e99-b0245dd2ffc5"
version = "1.7.2+0"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"
version = "1.11.0+1"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"
version = "1.11.0"

[[deps.Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "27ecae93dd25ee0909666e6835051dd684cc035e"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+2"

[[deps.Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll"]
git-tree-sha1 = "8be878062e0ffa2c3f67bb58a595375eda5de80b"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.11.0+0"

[[deps.Libglvnd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll", "Xorg_libXext_jll"]
git-tree-sha1 = "ff3b4b9d35de638936a525ecd36e86a8bb919d11"
uuid = "7e76a0d4-f3c7-5321-8279-8d96eeed0f29"
version = "1.7.0+0"

[[deps.Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "df37206100d39f79b3376afb6b9cee4970041c61"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.51.1+0"

[[deps.Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "be484f5c92fad0bd8acfef35fe017900b0b73809"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.18.0+0"

[[deps.Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "89211ea35d9df5831fca5d33552c02bd33878419"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.40.3+0"

[[deps.Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "XZ_jll", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "4ab7581296671007fc33f07a721631b8855f4b1d"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.7.1+0"

[[deps.Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "e888ad02ce716b319e6bdb985d2ef300e7089889"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.40.3+0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
version = "1.11.0"

[[deps.Loess]]
deps = ["Distances", "LinearAlgebra", "Statistics", "StatsAPI"]
git-tree-sha1 = "f749e7351f120b3566e5923fefdf8e52ba5ec7f9"
uuid = "4345ca2d-374a-55d4-8d30-97f9976e7612"
version = "0.6.4"

[[deps.LogExpFunctions]]
deps = ["DocStringExtensions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "13ca9e2586b89836fd20cccf56e57e2b9ae7f38f"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.29"

    [deps.LogExpFunctions.extensions]
    LogExpFunctionsChainRulesCoreExt = "ChainRulesCore"
    LogExpFunctionsChangesOfVariablesExt = "ChangesOfVariables"
    LogExpFunctionsInverseFunctionsExt = "InverseFunctions"

    [deps.LogExpFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ChangesOfVariables = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
    InverseFunctions = "3587e190-3f89-42d0-90ee-14403ec27112"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"
version = "1.11.0"

[[deps.LoweredCodeUtils]]
deps = ["JuliaInterpreter"]
git-tree-sha1 = "688d6d9e098109051ae33d126fcfc88c4ce4a021"
uuid = "6f1432cf-f94c-5a45-995e-cdbf5db27b0b"
version = "3.1.0"

[[deps.Lz4_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "191686b1ac1ea9c89fc52e996ad15d1d241d1e33"
uuid = "5ced341a-0733-55b8-9ab6-a4889d929147"
version = "1.10.1+0"

[[deps.MIMEs]]
git-tree-sha1 = "1833212fd6f580c20d4291da9c1b4e8a655b128e"
uuid = "6c6e2e6c-3030-632d-7369-2d6c69616d65"
version = "1.0.0"

[[deps.MKL_jll]]
deps = ["Artifacts", "IntelOpenMP_jll", "JLLWrappers", "LazyArtifacts", "Libdl", "oneTBB_jll"]
git-tree-sha1 = "5de60bc6cb3899cd318d80d627560fae2e2d99ae"
uuid = "856f044c-d86e-5d09-b602-aeab76dc8ba7"
version = "2025.0.1+1"

[[deps.MacroTools]]
git-tree-sha1 = "72aebe0b5051e5143a079a4685a46da330a40472"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.15"

[[deps.Makie]]
deps = ["Animations", "Base64", "CRC32c", "ColorBrewer", "ColorSchemes", "ColorTypes", "Colors", "Contour", "Dates", "DelaunayTriangulation", "Distributions", "DocStringExtensions", "Downloads", "FFMPEG_jll", "FileIO", "FilePaths", "FixedPointNumbers", "Format", "FreeType", "FreeTypeAbstraction", "GeometryBasics", "GridLayoutBase", "ImageBase", "ImageIO", "InteractiveUtils", "Interpolations", "IntervalSets", "InverseFunctions", "Isoband", "KernelDensity", "LaTeXStrings", "LinearAlgebra", "MacroTools", "MakieCore", "Markdown", "MathTeXEngine", "Observables", "OffsetArrays", "Packing", "PlotUtils", "PolygonOps", "PrecompileTools", "Printf", "REPL", "Random", "RelocatableFolders", "Scratch", "ShaderAbstractions", "Showoff", "SignedDistanceFields", "SparseArrays", "Statistics", "StatsBase", "StatsFuns", "StructArrays", "TriplotBase", "UnicodeFun", "Unitful"]
git-tree-sha1 = "be3051d08b78206fb5e688e8d70c9e84d0264117"
uuid = "ee78f7c6-11fb-53f2-987a-cfe4a2b5a57a"
version = "0.21.18"

[[deps.MakieCore]]
deps = ["ColorTypes", "GeometryBasics", "IntervalSets", "Observables"]
git-tree-sha1 = "9019b391d7d086e841cbeadc13511224bd029ab3"
uuid = "20f20a25-4f0e-4fdf-b5d1-57303727442b"
version = "0.8.12"

[[deps.MappedArrays]]
git-tree-sha1 = "2dab0221fe2b0f2cb6754eaa743cc266339f527e"
uuid = "dbb5928d-eab1-5f90-85c2-b9b0edb7c900"
version = "0.4.2"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"
version = "1.11.0"

[[deps.MathTeXEngine]]
deps = ["AbstractTrees", "Automa", "DataStructures", "FreeTypeAbstraction", "GeometryBasics", "LaTeXStrings", "REPL", "RelocatableFolders", "UnicodeFun"]
git-tree-sha1 = "f45c8916e8385976e1ccd055c9874560c257ab13"
uuid = "0a4f8689-d25c-4efe-a92b-7142dfc1aa53"
version = "0.6.2"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"
version = "2.28.6+0"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "ec4f7fbeab05d7747bdf98eb74d130a2a2ed298d"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.2.0"

[[deps.MixedModels]]
deps = ["Arrow", "BSplineKit", "Compat", "DataAPI", "Distributions", "GLM", "JSON3", "LinearAlgebra", "Markdown", "MixedModelsDatasets", "NLopt", "PooledArrays", "PrecompileTools", "ProgressMeter", "Random", "SparseArrays", "StaticArrays", "Statistics", "StatsAPI", "StatsBase", "StatsFuns", "StatsModels", "StructTypes", "Tables", "TypedTables"]
git-tree-sha1 = "ddf8edec197022e1d041dc301b4465f586b56ccc"
uuid = "ff71e718-51f3-5ec2-a782-8ffcbfa3c316"
version = "4.25.4"

[[deps.MixedModelsDatasets]]
deps = ["Arrow", "Artifacts", "LazyArtifacts"]
git-tree-sha1 = "ac0036e4f1829db000db46aad4cd5a207bba8465"
uuid = "7e9fb7ac-9f67-43bf-b2c8-96ba0796cbb6"
version = "0.1.2"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"
version = "1.11.0"

[[deps.Mocking]]
deps = ["Compat", "ExprTools"]
git-tree-sha1 = "2c140d60d7cb82badf06d8783800d0bcd1a7daa2"
uuid = "78c3b35d-d492-501b-9361-3d52fe80e533"
version = "0.8.1"

[[deps.MosaicViews]]
deps = ["MappedArrays", "OffsetArrays", "PaddedViews", "StackViews"]
git-tree-sha1 = "7b86a5d4d70a9f5cdf2dacb3cbe6d251d1a61dbe"
uuid = "e94cdb99-869f-56ef-bcf0-1ae2bcbe0389"
version = "0.3.4"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2023.12.12"

[[deps.MultivariateStats]]
deps = ["Arpack", "Distributions", "LinearAlgebra", "SparseArrays", "Statistics", "StatsAPI", "StatsBase"]
git-tree-sha1 = "816620e3aac93e5b5359e4fdaf23ca4525b00ddf"
uuid = "6f286f6a-111f-5878-ab1e-185364afe411"
version = "0.10.3"

[[deps.NLopt]]
deps = ["CEnum", "NLopt_jll"]
git-tree-sha1 = "ddb22a00a2dd27c98e0a94879544eb92d192184a"
uuid = "76087f3c-5699-56af-9a33-bf431cd00edd"
version = "1.1.3"

    [deps.NLopt.extensions]
    NLoptMathOptInterfaceExt = ["MathOptInterface"]

    [deps.NLopt.weakdeps]
    MathOptInterface = "b8f27783-ece8-5eb3-8dc8-9495eed66fee"

[[deps.NLopt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "b0154a615d5b2b6cf7a2501123b793577d0b9950"
uuid = "079eb43e-fd8e-5478-9966-2cf3e3edb778"
version = "2.10.0+0"

[[deps.NaNMath]]
deps = ["OpenLibm_jll"]
git-tree-sha1 = "cc0a5deefdb12ab3a096f00a6d42133af4560d71"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.1.2"

[[deps.NamedArrays]]
deps = ["Combinatorics", "DataStructures", "DelimitedFiles", "InvertedIndices", "LinearAlgebra", "Random", "Requires", "SparseArrays", "Statistics"]
git-tree-sha1 = "58e317b3b956b8aaddfd33ff4c3e33199cd8efce"
uuid = "86f7a689-2022-50b4-a561-43c23ac3c673"
version = "0.10.3"

[[deps.NaturalSort]]
git-tree-sha1 = "eda490d06b9f7c00752ee81cfa451efe55521e21"
uuid = "c020b1a1-e9b0-503a-9c33-f039bfc54a85"
version = "1.0.0"

[[deps.NearestNeighbors]]
deps = ["Distances", "StaticArrays"]
git-tree-sha1 = "8a3271d8309285f4db73b4f662b1b290c715e85e"
uuid = "b8a86587-4115-5ab1-83bc-aa920d37bbce"
version = "0.4.21"

[[deps.Netpbm]]
deps = ["FileIO", "ImageCore", "ImageMetadata"]
git-tree-sha1 = "d92b107dbb887293622df7697a2223f9f8176fcd"
uuid = "f09324ee-3d7c-5217-9330-fc30815ba969"
version = "1.1.1"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.Observables]]
git-tree-sha1 = "7438a59546cf62428fc9d1bc94729146d37a7225"
uuid = "510215fc-4207-5dde-b226-833fc4488ee2"
version = "0.5.5"

[[deps.OffsetArrays]]
git-tree-sha1 = "5e1897147d1ff8d98883cda2be2187dcf57d8f0c"
uuid = "6fe1bfb0-de20-5000-8ca7-80f57d26f881"
version = "1.15.0"
weakdeps = ["Adapt"]

    [deps.OffsetArrays.extensions]
    OffsetArraysAdaptExt = "Adapt"

[[deps.Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[deps.OpenBLASConsistentFPCSR_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "567515ca155d0020a45b05175449b499c63e7015"
uuid = "6cdc7f73-28fd-5e50-80fb-958a8875b1af"
version = "0.3.29+0"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.27+1"

[[deps.OpenEXR]]
deps = ["Colors", "FileIO", "OpenEXR_jll"]
git-tree-sha1 = "97db9e07fe2091882c765380ef58ec553074e9c7"
uuid = "52e1d378-f018-4a11-a4be-720524705ac7"
version = "0.3.3"

[[deps.OpenEXR_jll]]
deps = ["Artifacts", "Imath_jll", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "8292dd5c8a38257111ada2174000a33745b06d4e"
uuid = "18a262bb-aa17-5467-a713-aee519bc75cb"
version = "3.2.4+0"

[[deps.OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"
version = "0.8.1+2"

[[deps.OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "a9697f1d06cc3eb3fb3ad49cc67f2cfabaac31ea"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "3.0.16+0"

[[deps.OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "1346c9208249809840c91b26703912dff463d335"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.6+0"

[[deps.Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6703a85cb3781bd5909d48730a67205f3f31a575"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.3+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "cc4054e898b852042d7b503313f7ad03de99c3dd"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.8.0"

[[deps.PCRE2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "efcefdf7-47ab-520b-bdef-62a2eaa19f15"
version = "10.42.0+1"

[[deps.PDMats]]
deps = ["LinearAlgebra", "SparseArrays", "SuiteSparse"]
git-tree-sha1 = "966b85253e959ea89c53a9abebbf2e964fbf593b"
uuid = "90014a1f-27ba-587c-ab20-58faa44d9150"
version = "0.11.32"

[[deps.PNGFiles]]
deps = ["Base64", "CEnum", "ImageCore", "IndirectArrays", "OffsetArrays", "libpng_jll"]
git-tree-sha1 = "cf181f0b1e6a18dfeb0ee8acc4a9d1672499626c"
uuid = "f57f5aa1-a3ce-4bc8-8ab9-96f992907883"
version = "0.4.4"

[[deps.Packing]]
deps = ["GeometryBasics"]
git-tree-sha1 = "bc5bf2ea3d5351edf285a06b0016788a121ce92c"
uuid = "19eb6ba3-879d-56ad-ad62-d5c202156566"
version = "0.5.1"

[[deps.PaddedViews]]
deps = ["OffsetArrays"]
git-tree-sha1 = "0fac6313486baae819364c52b4f483450a9d793f"
uuid = "5432bcbf-9aad-5242-b902-cca2824c8663"
version = "0.5.12"

[[deps.Pango_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "FriBidi_jll", "Glib_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl"]
git-tree-sha1 = "3b31172c032a1def20c98dae3f2cdc9d10e3b561"
uuid = "36c8627f-9965-5494-a995-c6b170f724f3"
version = "1.56.1+0"

[[deps.Parsers]]
deps = ["Dates", "PrecompileTools", "UUIDs"]
git-tree-sha1 = "8489905bcdbcfac64d1daa51ca07c0d8f0283821"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.8.1"

[[deps.Pixman_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "LLVMOpenMP_jll", "Libdl"]
git-tree-sha1 = "35621f10a7531bc8fa58f74610b1bfb70a3cfc6b"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.43.4+0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "FileWatching", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "Random", "SHA", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.11.0"
weakdeps = ["REPL"]

    [deps.Pkg.extensions]
    REPLExt = "REPL"

[[deps.PkgVersion]]
deps = ["Pkg"]
git-tree-sha1 = "f9501cc0430a26bc3d156ae1b5b0c1b47af4d6da"
uuid = "eebad327-c553-4316-9ea0-9fa01ccd7688"
version = "0.3.3"

[[deps.PlotUtils]]
deps = ["ColorSchemes", "Colors", "Dates", "PrecompileTools", "Printf", "Random", "Reexport", "StableRNGs", "Statistics"]
git-tree-sha1 = "3ca9a356cd2e113c420f2c13bea19f8d3fb1cb18"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.4.3"

[[deps.PlutoHooks]]
deps = ["InteractiveUtils", "Markdown", "UUIDs"]
git-tree-sha1 = "072cdf20c9b0507fdd977d7d246d90030609674b"
uuid = "0ff47ea0-7a50-410d-8455-4348d5de0774"
version = "0.0.5"

[[deps.PlutoLinks]]
deps = ["FileWatching", "InteractiveUtils", "Markdown", "PlutoHooks", "Revise", "UUIDs"]
git-tree-sha1 = "8f5fa7056e6dcfb23ac5211de38e6c03f6367794"
uuid = "0ff47ea0-7a50-410d-8455-4348d5de0420"
version = "0.1.6"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "FixedPointNumbers", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "MIMEs", "Markdown", "Random", "Reexport", "URIs", "UUIDs"]
git-tree-sha1 = "7e71a55b87222942f0f9337be62e26b1f103d3e4"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.61"

[[deps.PolygonOps]]
git-tree-sha1 = "77b3d3605fc1cd0b42d95eba87dfcd2bf67d5ff6"
uuid = "647866c9-e3ac-4575-94e7-e3d426903924"
version = "0.1.2"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "36d8b4b899628fb92c2749eb488d884a926614d3"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.3"

[[deps.PrecompileTools]]
deps = ["Preferences"]
git-tree-sha1 = "5aa36f7049a63a1528fe8f7c3f2113413ffd4e1f"
uuid = "aea7be01-6a6a-4083-8856-8a6e6704d82a"
version = "1.2.1"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "9306f6085165d270f7e3db02af26a400d580f5c6"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.4.3"

[[deps.PrettyTables]]
deps = ["Crayons", "LaTeXStrings", "Markdown", "PrecompileTools", "Printf", "Reexport", "StringManipulation", "Tables"]
git-tree-sha1 = "1101cd475833706e4d0e7b122218257178f48f34"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.4.0"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"
version = "1.11.0"

[[deps.ProgressMeter]]
deps = ["Distributed", "Printf"]
git-tree-sha1 = "8f6bc219586aef8baf0ff9a5fe16ee9c70cb65e4"
uuid = "92933f4c-e287-5a05-a399-4b506db050ca"
version = "1.10.2"

[[deps.PtrArrays]]
git-tree-sha1 = "1d36ef11a9aaf1e8b74dacc6a731dd1de8fd493d"
uuid = "43287f4e-b6f4-7ad1-bb20-aadabca52c3d"
version = "1.3.0"

[[deps.QOI]]
deps = ["ColorTypes", "FileIO", "FixedPointNumbers"]
git-tree-sha1 = "8b3fc30bc0390abdce15f8822c889f669baed73d"
uuid = "4b34888f-f399-49d4-9bb3-47ed5cae4e65"
version = "1.0.1"

[[deps.QuadGK]]
deps = ["DataStructures", "LinearAlgebra"]
git-tree-sha1 = "9da16da70037ba9d701192e27befedefb91ec284"
uuid = "1fd47b50-473d-5c70-9696-f719f8f3bcdc"
version = "2.11.2"

    [deps.QuadGK.extensions]
    QuadGKEnzymeExt = "Enzyme"

    [deps.QuadGK.weakdeps]
    Enzyme = "7da242da-08ed-463a-9acd-ee780be4f1d9"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "StyledStrings", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"
version = "1.11.0"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
version = "1.11.0"

[[deps.RangeArrays]]
git-tree-sha1 = "b9039e93773ddcfc828f12aadf7115b4b4d225f5"
uuid = "b3c3ace0-ae52-54e7-9d0b-2c1406fd6b9d"
version = "0.3.2"

[[deps.Ratios]]
deps = ["Requires"]
git-tree-sha1 = "1342a47bf3260ee108163042310d26f2be5ec90b"
uuid = "c84ed2f1-dad5-54f0-aa8e-dbefe2724439"
version = "0.4.5"
weakdeps = ["FixedPointNumbers"]

    [deps.Ratios.extensions]
    RatiosFixedPointNumbersExt = "FixedPointNumbers"

[[deps.RecipesBase]]
deps = ["PrecompileTools"]
git-tree-sha1 = "5c3d09cc4f31f5fc6af001c250bf1278733100ff"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.3.4"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.RelocatableFolders]]
deps = ["SHA", "Scratch"]
git-tree-sha1 = "ffdaf70d81cf6ff22c2b6e733c900c3321cab864"
uuid = "05181044-ff0b-4ac5-8273-598c1e38db00"
version = "1.0.1"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "62389eeff14780bfe55195b7204c0d8738436d64"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.1"

[[deps.Revise]]
deps = ["CodeTracking", "FileWatching", "JuliaInterpreter", "LibGit2", "LoweredCodeUtils", "OrderedCollections", "REPL", "Requires", "UUIDs", "Unicode"]
git-tree-sha1 = "9bb80533cb9769933954ea4ffbecb3025a783198"
uuid = "295af30f-e4ad-537b-8983-00126c2a3abe"
version = "3.7.2"
weakdeps = ["Distributed"]

    [deps.Revise.extensions]
    DistributedExt = "Distributed"

[[deps.Rmath]]
deps = ["Random", "Rmath_jll"]
git-tree-sha1 = "852bd0f55565a9e973fcfee83a84413270224dc4"
uuid = "79098fc4-a85e-5d69-aa6a-4863f24498fa"
version = "0.8.0"

[[deps.Rmath_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "58cdd8fb2201a6267e1db87ff148dd6c1dbd8ad8"
uuid = "f50d1b31-88e8-58de-be2c-1cc44531875f"
version = "0.5.1+0"

[[deps.Roots]]
deps = ["Accessors", "CommonSolve", "Printf"]
git-tree-sha1 = "442b4353ee8c26756672afb2db81894fc28811f3"
uuid = "f2b01f46-fcfa-551c-844a-d8ac1e96c665"
version = "2.2.6"

    [deps.Roots.extensions]
    RootsChainRulesCoreExt = "ChainRulesCore"
    RootsForwardDiffExt = "ForwardDiff"
    RootsIntervalRootFindingExt = "IntervalRootFinding"
    RootsSymPyExt = "SymPy"
    RootsSymPyPythonCallExt = "SymPyPythonCall"

    [deps.Roots.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ForwardDiff = "f6369f11-7733-5829-9624-2563aa707210"
    IntervalRootFinding = "d2bf35a9-74e0-55ec-b149-d360ff49b807"
    SymPy = "24249f21-da20-56a4-8eb1-6a02cf4ae2e6"
    SymPyPythonCall = "bc8888f7-b21e-4b7c-a06a-5d9c9496438c"

[[deps.RoundingEmulator]]
git-tree-sha1 = "40b9edad2e5287e05bd413a38f61a8ff55b9557b"
uuid = "5eaf0fd0-dfba-4ccb-bf02-d820a40db705"
version = "0.2.1"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.SIMD]]
deps = ["PrecompileTools"]
git-tree-sha1 = "fea870727142270bdf7624ad675901a1ee3b4c87"
uuid = "fdea26ae-647d-5447-a871-4b548cad5224"
version = "3.7.1"

[[deps.Scratch]]
deps = ["Dates"]
git-tree-sha1 = "3bac05bc7e74a75fd9cba4295cde4045d9fe2386"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.2.1"

[[deps.SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "712fb0231ee6f9120e005ccd56297abbc053e7e0"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.4.8"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
version = "1.11.0"

[[deps.ShaderAbstractions]]
deps = ["ColorTypes", "FixedPointNumbers", "GeometryBasics", "LinearAlgebra", "Observables", "StaticArrays", "StructArrays", "Tables"]
git-tree-sha1 = "79123bc60c5507f035e6d1d9e563bb2971954ec8"
uuid = "65257c39-d410-5151-9873-9b3e5be5013e"
version = "0.4.1"

[[deps.SharedArrays]]
deps = ["Distributed", "Mmap", "Random", "Serialization"]
uuid = "1a1011a3-84de-559e-8e89-a11a2f7dc383"
version = "1.11.0"

[[deps.ShiftedArrays]]
git-tree-sha1 = "503688b59397b3307443af35cd953a13e8005c16"
uuid = "1277b4bf-5013-50f5-be3d-901d8477a67a"
version = "2.0.0"

[[deps.Showoff]]
deps = ["Dates", "Grisu"]
git-tree-sha1 = "91eddf657aca81df9ae6ceb20b959ae5653ad1de"
uuid = "992d4aef-0814-514b-bc4d-f2e9a6c4116f"
version = "1.0.3"

[[deps.SignedDistanceFields]]
deps = ["Random", "Statistics", "Test"]
git-tree-sha1 = "d263a08ec505853a5ff1c1ebde2070419e3f28e9"
uuid = "73760f76-fbc4-59ce-8f25-708e95d2df96"
version = "0.4.0"

[[deps.SimpleTraits]]
deps = ["InteractiveUtils", "MacroTools"]
git-tree-sha1 = "5d7e3f4e11935503d3ecaf7186eac40602e7d231"
uuid = "699a6c99-e7fa-54fc-8d76-47d257e15c1d"
version = "0.9.4"

[[deps.Sixel]]
deps = ["Dates", "FileIO", "ImageCore", "IndirectArrays", "OffsetArrays", "REPL", "libsixel_jll"]
git-tree-sha1 = "2da10356e31327c7096832eb9cd86307a50b1eb6"
uuid = "45858cf5-a6b0-47a3-bbea-62219f50df47"
version = "0.1.3"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"
version = "1.11.0"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "66e0a8e672a0bdfca2c3f5937efb8538b9ddc085"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.2.1"

[[deps.SparseArrays]]
deps = ["Libdl", "LinearAlgebra", "Random", "Serialization", "SuiteSparse_jll"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
version = "1.11.0"

[[deps.SpecialFunctions]]
deps = ["IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "64cca0c26b4f31ba18f13f6c12af7c85f478cfde"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.5.0"
weakdeps = ["ChainRulesCore"]

    [deps.SpecialFunctions.extensions]
    SpecialFunctionsChainRulesCoreExt = "ChainRulesCore"

[[deps.SplitApplyCombine]]
deps = ["Dictionaries", "Indexing"]
git-tree-sha1 = "c06d695d51cfb2187e6848e98d6252df9101c588"
uuid = "03a91e81-4c3e-53e1-a0a4-9c0c8f19dd66"
version = "1.2.3"

[[deps.StableRNGs]]
deps = ["Random"]
git-tree-sha1 = "83e6cce8324d49dfaf9ef059227f91ed4441a8e5"
uuid = "860ef19b-820b-49d6-a774-d7a799459cd3"
version = "1.0.2"

[[deps.StackViews]]
deps = ["OffsetArrays"]
git-tree-sha1 = "46e589465204cd0c08b4bd97385e4fa79a0c770c"
uuid = "cae243ae-269e-4f55-b966-ac2d0dc13c15"
version = "0.1.1"

[[deps.Static]]
deps = ["CommonWorldInvalidations", "IfElse", "PrecompileTools"]
git-tree-sha1 = "f737d444cb0ad07e61b3c1bef8eb91203c321eff"
uuid = "aedffcd0-7271-4cad-89d0-dc628f76c6d3"
version = "1.2.0"

[[deps.StaticArrays]]
deps = ["LinearAlgebra", "PrecompileTools", "Random", "StaticArraysCore"]
git-tree-sha1 = "0feb6b9031bd5c51f9072393eb5ab3efd31bf9e4"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.9.13"
weakdeps = ["ChainRulesCore", "Statistics"]

    [deps.StaticArrays.extensions]
    StaticArraysChainRulesCoreExt = "ChainRulesCore"
    StaticArraysStatisticsExt = "Statistics"

[[deps.StaticArraysCore]]
git-tree-sha1 = "192954ef1208c7019899fbf8049e717f92959682"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.3"

[[deps.Statistics]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "ae3bb1eb3bba077cd276bc5cfc337cc65c3075c0"
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.11.1"
weakdeps = ["SparseArrays"]

    [deps.Statistics.extensions]
    SparseArraysExt = ["SparseArrays"]

[[deps.StatsAPI]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1ff449ad350c9c4cbc756624d6f8a8c3ef56d3ed"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.7.0"

[[deps.StatsBase]]
deps = ["AliasTables", "DataAPI", "DataStructures", "LinearAlgebra", "LogExpFunctions", "Missings", "Printf", "Random", "SortingAlgorithms", "SparseArrays", "Statistics", "StatsAPI"]
git-tree-sha1 = "29321314c920c26684834965ec2ce0dacc9cf8e5"
uuid = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
version = "0.34.4"

[[deps.StatsFuns]]
deps = ["HypergeometricFunctions", "IrrationalConstants", "LogExpFunctions", "Reexport", "Rmath", "SpecialFunctions"]
git-tree-sha1 = "b423576adc27097764a90e163157bcfc9acf0f46"
uuid = "4c63d2b9-4356-54db-8cca-17b64c39e42c"
version = "1.3.2"
weakdeps = ["ChainRulesCore", "InverseFunctions"]

    [deps.StatsFuns.extensions]
    StatsFunsChainRulesCoreExt = "ChainRulesCore"
    StatsFunsInverseFunctionsExt = "InverseFunctions"

[[deps.StatsKit]]
deps = ["Bootstrap", "CSV", "CategoricalArrays", "Clustering", "DataFrames", "Distances", "Distributions", "GLM", "HypothesisTests", "KernelDensity", "Loess", "MixedModels", "MultivariateStats", "Reexport", "ShiftedArrays", "Statistics", "StatsBase", "TimeSeries"]
git-tree-sha1 = "9888fa88a0ea16dd397af86d906fee56f4d1dd06"
uuid = "2cb19f9e-ec4d-5c53-8573-a4542a68d3f0"
version = "0.3.1"

[[deps.StatsModels]]
deps = ["DataAPI", "DataStructures", "LinearAlgebra", "Printf", "REPL", "ShiftedArrays", "SparseArrays", "StatsAPI", "StatsBase", "StatsFuns", "Tables"]
git-tree-sha1 = "9022bcaa2fc1d484f1326eaa4db8db543ca8c66d"
uuid = "3eaba693-59b7-5ba5-a881-562e759f1c8d"
version = "0.7.4"

[[deps.StringManipulation]]
deps = ["PrecompileTools"]
git-tree-sha1 = "725421ae8e530ec29bcbdddbe91ff8053421d023"
uuid = "892a3eda-7b42-436c-8928-eab12a02cf0e"
version = "0.4.1"

[[deps.StringViews]]
git-tree-sha1 = "ec4bf39f7d25db401bcab2f11d2929798c0578e5"
uuid = "354b36f9-a18e-4713-926e-db85100087ba"
version = "1.3.4"

[[deps.StructArrays]]
deps = ["ConstructionBase", "DataAPI", "Tables"]
git-tree-sha1 = "9537ef82c42cdd8c5d443cbc359110cbb36bae10"
uuid = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
version = "0.6.21"

    [deps.StructArrays.extensions]
    StructArraysAdaptExt = "Adapt"
    StructArraysGPUArraysCoreExt = ["GPUArraysCore", "KernelAbstractions"]
    StructArraysLinearAlgebraExt = "LinearAlgebra"
    StructArraysSparseArraysExt = "SparseArrays"
    StructArraysStaticArraysExt = "StaticArrays"

    [deps.StructArrays.weakdeps]
    Adapt = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
    GPUArraysCore = "46192b85-c4d5-4398-a991-12ede77f4527"
    KernelAbstractions = "63c18a36-062a-441e-b654-da1e3ab1ce7c"
    LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
    SparseArrays = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
    StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[[deps.StructTypes]]
deps = ["Dates", "UUIDs"]
git-tree-sha1 = "159331b30e94d7b11379037feeb9b690950cace8"
uuid = "856f2bd8-1eba-4b0a-8007-ebc267875bd4"
version = "1.11.0"

[[deps.StyledStrings]]
uuid = "f489334b-da3d-4c2e-b8f0-e476e12c162b"
version = "1.11.0"

[[deps.SuiteSparse]]
deps = ["Libdl", "LinearAlgebra", "Serialization", "SparseArrays"]
uuid = "4607b0f0-06f3-5cda-b6b1-a6196a1729e9"

[[deps.SuiteSparse_jll]]
deps = ["Artifacts", "Libdl", "libblastrampoline_jll"]
uuid = "bea87d4a-7f5b-5778-9afe-8cc45184846c"
version = "7.7.0+0"

[[deps.SwarmMakie]]
deps = ["KernelDensity", "Makie", "Random", "StatsBase"]
git-tree-sha1 = "41d45cd0801aa64c1d489dd10f62a201f8645e3e"
uuid = "0b1c068e-6a84-4e66-8136-5c95cafa83ed"
version = "0.1.3"
weakdeps = ["AlgebraOfGraphics"]

    [deps.SwarmMakie.extensions]
    AlgebraOfGraphicsExt = "AlgebraOfGraphics"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.3"

[[deps.TZJData]]
deps = ["Artifacts"]
git-tree-sha1 = "7def47e953a91cdcebd08fbe76d69d2715499a7d"
uuid = "dc5dba14-91b3-4cab-a142-028a31da12f7"
version = "1.4.0+2025a"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "OrderedCollections", "TableTraits"]
git-tree-sha1 = "598cd7c1f68d1e205689b1c2fe65a9f85846f297"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.12.0"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"
version = "1.10.0"

[[deps.TensorCore]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1feb45f88d133a655e001435632f019a9a1bcdb6"
uuid = "62fd8b95-f654-4bbd-a8a5-9c27f68ccd50"
version = "0.1.1"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"
version = "1.11.0"

[[deps.TiffImages]]
deps = ["ColorTypes", "DataStructures", "DocStringExtensions", "FileIO", "FixedPointNumbers", "IndirectArrays", "Inflate", "Mmap", "OffsetArrays", "PkgVersion", "ProgressMeter", "SIMD", "UUIDs"]
git-tree-sha1 = "f21231b166166bebc73b99cea236071eb047525b"
uuid = "731e570b-9d59-4bfa-96dc-6df516fadf69"
version = "0.11.3"

[[deps.TimeSeries]]
deps = ["Dates", "DelimitedFiles", "DocStringExtensions", "IteratorInterfaceExtensions", "PrettyTables", "RecipesBase", "Reexport", "Statistics", "TableTraits", "Tables"]
git-tree-sha1 = "b0915b2d6032bab5d8c9424a37bc621500a67df9"
uuid = "9e3dc215-6440-5c97-bce1-76c03772f85e"
version = "0.24.2"

[[deps.TimeZones]]
deps = ["Artifacts", "Dates", "Downloads", "InlineStrings", "Mocking", "Printf", "Scratch", "TZJData", "Unicode", "p7zip_jll"]
git-tree-sha1 = "2c705e96825b66c4a3f25031a683c06518256dd3"
uuid = "f269a46b-ccf7-5d73-abea-4c690281aa53"
version = "1.21.3"
weakdeps = ["RecipesBase"]

    [deps.TimeZones.extensions]
    TimeZonesRecipesBaseExt = "RecipesBase"

[[deps.TranscodingStreams]]
git-tree-sha1 = "0c45878dcfdcfa8480052b6ab162cdd138781742"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.11.3"

[[deps.Tricks]]
git-tree-sha1 = "6cae795a5a9313bbb4f60683f7263318fc7d1505"
uuid = "410a4b4d-49e4-4fbc-ab6d-cb71b17b3775"
version = "0.1.10"

[[deps.TriplotBase]]
git-tree-sha1 = "4d4ed7f294cda19382ff7de4c137d24d16adc89b"
uuid = "981d1d27-644d-49a2-9326-4793e63143c3"
version = "0.1.0"

[[deps.TypedTables]]
deps = ["Adapt", "Dictionaries", "Indexing", "SplitApplyCombine", "Tables", "Unicode"]
git-tree-sha1 = "84fd7dadde577e01eb4323b7e7b9cb51c62c60d4"
uuid = "9d95f2ec-7b3d-5a63-8d20-e2491e220bb9"
version = "1.4.6"

[[deps.URIs]]
git-tree-sha1 = "67db6cc7b3821e19ebe75791a9dd19c9b1188f2b"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.5.1"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"
version = "1.11.0"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"
version = "1.11.0"

[[deps.UnicodeFun]]
deps = ["REPL"]
git-tree-sha1 = "53915e50200959667e78a92a418594b428dffddf"
uuid = "1cfade01-22cf-5700-b092-accc4b62d6e1"
version = "0.4.1"

[[deps.Unitful]]
deps = ["Dates", "LinearAlgebra", "Random"]
git-tree-sha1 = "c0667a8e676c53d390a09dc6870b3d8d6650e2bf"
uuid = "1986cc42-f94f-5a68-af5c-568840ba703d"
version = "1.22.0"
weakdeps = ["ConstructionBase", "InverseFunctions"]

    [deps.Unitful.extensions]
    ConstructionBaseUnitfulExt = "ConstructionBase"
    InverseFunctionsUnitfulExt = "InverseFunctions"

[[deps.WeakRefStrings]]
deps = ["DataAPI", "InlineStrings", "Parsers"]
git-tree-sha1 = "b1be2855ed9ed8eac54e5caff2afcdb442d52c23"
uuid = "ea10d353-3f73-51f8-a26c-33c1cb351aa5"
version = "1.4.2"

[[deps.WebP]]
deps = ["CEnum", "ColorTypes", "FileIO", "FixedPointNumbers", "ImageCore", "libwebp_jll"]
git-tree-sha1 = "aa1ca3c47f119fbdae8770c29820e5e6119b83f2"
uuid = "e3aaa7dc-3e4b-44e0-be63-ffb868ccd7c1"
version = "0.1.3"

[[deps.WoodburyMatrices]]
deps = ["LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "c1a7aa6219628fcd757dede0ca95e245c5cd9511"
uuid = "efce3f68-66dc-5838-9240-27a6d6f5f9b6"
version = "1.0.0"

[[deps.WorkerUtilities]]
git-tree-sha1 = "cd1659ba0d57b71a464a29e64dbc67cfe83d54e7"
uuid = "76eceee3-57b5-4d4a-8e66-0e911cebbf60"
version = "1.6.1"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Zlib_jll"]
git-tree-sha1 = "b8b243e47228b4a3877f1dd6aee0c5d56db7fcf4"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.13.6+1"

[[deps.XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "7d1671acbe47ac88e981868a078bd6b4e27c5191"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.42+0"

[[deps.XZ_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "56c6604ec8b2d82cc4cfe01aa03b00426aac7e1f"
uuid = "ffd25f8a-64ca-5728-b0f7-c24cf3aae800"
version = "5.6.4+1"

[[deps.Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "9dafcee1d24c4f024e7edc92603cedba72118283"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.8.6+3"

[[deps.Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "e9216fdcd8514b7072b43653874fd688e4c6c003"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.12+0"

[[deps.Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "89799ae67c17caa5b3b5a19b8469eeee474377db"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.5+0"

[[deps.Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "d7155fea91a4123ef59f42c4afb5ab3b4ca95058"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.6+3"

[[deps.Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Xorg_libX11_jll"]
git-tree-sha1 = "a490c6212a0e90d2d55111ac956f7c4fa9c277a6"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.11+1"

[[deps.Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "c57201109a9e4c0585b208bb408bc41d205ac4e9"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.2+0"

[[deps.Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "1a74296303b6524a0472a8cb12d3d87a78eb3612"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.17.0+3"

[[deps.Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6dba04dbfb72ae3ebe5418ba33d087ba8aa8cb00"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.5.1+0"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.13+1"

[[deps.Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "446b23e73536f84e8037f5dce465e92275f6a308"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.7+1"

[[deps.isoband_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51b5eeb3f98367157a7a12a1fb0aa5328946c03c"
uuid = "9a68df92-36a6-505f-a73e-abb412b6bfb4"
version = "0.2.3+0"

[[deps.libaom_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "522c1df09d05a71785765d19c9524661234738e9"
uuid = "a4ae2306-e953-59d6-aa16-d00cac43593b"
version = "3.11.0+0"

[[deps.libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "e17c115d55c5fbb7e52ebedb427a0dca79d4484e"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.2+0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.11.0+0"

[[deps.libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "8a22cf860a7d27e4f3498a0fe0811a7957badb38"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.3+0"

[[deps.libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Zlib_jll"]
git-tree-sha1 = "068dfe202b0a05b8332f1e8e6b4080684b9c7700"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.47+0"

[[deps.libsixel_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "Libdl", "libpng_jll"]
git-tree-sha1 = "c1733e347283df07689d71d61e14be986e49e47a"
uuid = "075b6546-f08a-558a-be8f-8157d0f608a5"
version = "1.10.5+0"

[[deps.libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "490376214c4721cdaca654041f635213c6165cb3"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+2"

[[deps.libwebp_jll]]
deps = ["Artifacts", "Giflib_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libglvnd_jll", "Libtiff_jll", "libpng_jll"]
git-tree-sha1 = "d2408cac540942921e7bd77272c32e58c33d8a77"
uuid = "c5f90fcd-3b7e-5836-afba-fc50a0988cb2"
version = "1.5.0+0"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.59.0+0"

[[deps.oneTBB_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "d5a767a3bb77135a99e433afe0eb14cd7f6914c3"
uuid = "1317d2d5-d96f-522e-a858-c73665f53c3e"
version = "2022.0.0+0"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+2"

[[deps.x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "14cc7083fc6dff3cc44f2bc435ee96d06ed79aa7"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "10164.0.1+0"

[[deps.x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "dcc541bb19ed5b0ede95581fb2e41ecf179527d2"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.6.0+0"
"""

# ╔═╡ Cell order:
# ╠═974ac923-a511-4128-ad12-b75ccdacec8c
# ╟─93a5e415-4b56-4edd-b59c-c00ce4be8612
# ╟─d8364382-e421-4be6-9854-ab43cdfc0b9e
# ╠═006bac1e-2965-4c35-b243-1035418c4a48
# ╟─a85aa103-5f00-4d3a-808a-be678b64500a
# ╠═76edb6c5-01aa-4457-b340-876663bd91ac
# ╠═bbdb9c4b-9946-4563-899a-db559357787c
# ╠═904eb0d6-aba7-43d5-9032-e910f80700bd
# ╠═886642fd-ab1d-480c-b204-86909a7193c3
# ╟─6fa2360d-cae0-4d1d-83fb-9cca1d471514
# ╟─09b57e4d-7400-47ee-9981-f6f39eb128f1
# ╟─28aa8578-f4c1-4215-9fab-e44193073dba
# ╠═abc0d0ea-27e6-4bad-bc95-9d6539ab4f44
# ╟─60e21bd4-779e-440b-8a7a-805eca8690c5
# ╟─f1ffa2d0-0449-4f28-af5e-25399e5eef99
# ╟─69a74948-61ff-498b-a6e5-260a3c40d2b0
# ╟─1dece7d4-b4a4-4c1d-a5b2-81f1e14716c0
# ╟─e5d38326-108b-4c4d-ad77-5be3597123c5
# ╠═a10c2e29-aa43-4384-9cc9-ea156806a9ab
# ╠═edefac7e-d9b7-4302-8b53-b451d8115498
# ╟─59dcf3ba-53fa-448b-b8de-25f981ee7d5f
# ╟─8b7ae46c-fc53-4759-b3de-81584f43723c
# ╟─9b527f31-41c3-4dad-bc71-a7ff9fa03ddc
# ╟─a08c1ce2-a892-418a-9d4a-d5208afa78c4
# ╟─f3164cd7-9970-4b28-9585-f52b6cc66d3f
# ╟─fdb2b9af-d1db-49d6-96de-7cee8a99784c
# ╟─bb5ec6d8-85b0-4cbd-b7bf-f259b3ed0ca1
# ╟─1713a0fc-a039-428e-b565-194e47055596
# ╟─b5c9beb8-c42a-43ab-b052-38c4f42f46b4
# ╟─a31295ec-e760-458d-9097-6cf934653301
# ╟─181dc4fa-12cb-432a-903f-2d8199151eb1
# ╟─7be514fb-bd0d-40a6-878f-3eb61f60d1fd
# ╠═ce0d1b8b-39cb-4843-ba67-6ac1aad2feea
# ╠═5921272a-a993-4b43-9137-fd06f0c4734f
# ╟─686b24b0-78a9-42b5-9d1d-845b8ffe0258
# ╟─1cd020b9-dd14-42ce-8dbc-59a3ff3377e6
# ╟─d555960f-0147-478d-8b94-da0880e40ae2
# ╟─7f4591ea-ec7a-4d9a-a421-20fb4b8b977a
# ╟─67277703-4a0b-4ffc-a00b-aec728b73f37
# ╟─c13f6c71-1831-4d26-a726-f61b46ad7b78
# ╟─aa687140-0390-444d-bca4-f35b995decb8
# ╟─b938307f-1431-4062-8f21-f1d839de3aab
# ╟─b97d840c-0d89-481b-82f1-1a85c841163b
# ╟─ae17e013-c5fe-46b6-856a-a9e30ddeef89
# ╟─3f2ceb2a-a14a-4502-8bf0-2d311d4d4062
# ╟─a37fc0b7-0a1a-456c-9b59-4429a8913afc
# ╟─7d536e79-d41c-4797-9484-a68002fcf4c7
# ╟─aff33a67-33d6-4538-b797-a517d3346507
# ╟─f63d0de8-681e-4f79-b73e-9dd6bb765715
# ╟─37564825-7961-4ce0-9c23-1481a8c445c2
# ╟─9e5c5e5e-aef7-4159-ab87-999d16a8279f
# ╟─8a226c19-6749-44a8-99c1-1a5653dd41a2
# ╟─773f817c-f1eb-4cfd-ae9a-61572b84911e
# ╟─0db6734d-a0e8-4072-b3f6-7ebb602b8d02
# ╟─07de8b5b-00b8-4150-9cb1-fc5557e8d648
# ╟─84f54ff6-4ff3-4b32-be3b-6e4a6a9350cc
# ╟─6ad727af-d324-4cb0-97b9-f574a173c164
# ╟─d72fdc5d-fa81-4a70-9024-dd2d092c3fdd
# ╟─d7db3e77-236c-4d36-b7df-aeb136f16a1d
# ╟─5dbb3baa-6178-4d6c-aa91-ecae89f4f6ac
# ╟─f2d58bb1-39e4-49f1-baa6-e2c8ac7979c2
# ╟─23ac280a-d127-4dd7-812b-f7c4b07ee28d
# ╟─22fe4993-0efa-4cbe-8ff3-4354d3c3378e
# ╟─9840aae1-fcc0-41fd-b3e7-3c2e52d0571e
# ╠═b3198067-2518-40cb-a2d6-c78ca87b54e8
# ╠═ad33d56d-c4eb-446e-94b2-0e1fde12433f
# ╠═9be26989-da51-476a-ba42-deb2e2c0d54c
# ╠═28f17b18-f527-49e1-83f9-6962e688693a
# ╠═4142602d-c97e-474c-95fb-628e7d821fcc
# ╠═cf3a3688-26f7-4870-86e7-06266ccd27ef
# ╠═8ebdbc8c-4b63-40ba-99fa-868196785854
# ╠═4e9ed283-274c-4d43-9d99-ebf90ab67501
# ╠═4b690c44-0cdc-41aa-b22d-3b4c4878a31f
# ╠═0111bedd-7c8c-419e-aafc-e7b3181dab1a
# ╟─ec41ea88-708e-4b0a-a995-5f4f356b1f18
# ╠═41d674c9-2cf5-43c8-887c-19b2caf10321
# ╠═c85e3b88-aeef-447c-b47e-7e6660d9c8ca
# ╟─5d095859-f032-4b8c-bfc1-441c91ec7fb8
# ╟─271eea30-91c6-4204-9958-e59b241f355a
# ╟─922bc3cc-e4b8-4073-87c1-a2d101c7eed5
# ╟─c8dad05d-3e21-435a-a3d9-4d4e0bc389c0
# ╟─35a6155e-69d0-46fa-9515-45c2a72aafc4
# ╟─1901b368-a426-4027-92e7-e0942abd9e82
# ╟─5b0ce1fb-ca29-497f-be42-c5e1a2462eaa
# ╠═7241f6dd-43d8-438a-bc2b-7515c0f7b7cc
# ╟─48384a51-2f58-4d94-893f-9cab73d29789
# ╟─00deb41f-ddd5-4e8a-8e66-ed640f9e0e15
# ╟─10487a7f-1ef4-496e-a48f-1149a1c0cf08
# ╠═769382aa-3666-402a-a6c8-5b934ce6c69d
# ╠═8f6826a5-b8f9-40b4-a793-a67028c6dea5
# ╠═e8ed396a-a0a3-4080-9cf5-0b5402253cb5
# ╠═4c0fcdf3-4df4-463b-ab53-538e9c3fd9f9
# ╠═e183e449-d112-4317-b987-84f5980cfc65
# ╠═646d39a4-b107-4b6b-b667-842bcd25e502
# ╠═10d48fb5-d1f1-4cb3-8087-368e42cf8bf6
# ╠═0d460887-d910-4b05-aea8-d24f390e4833
# ╠═4376aecc-078e-4c20-ab4f-1bd0843dea07
# ╟─522e5282-712f-4d80-a379-08ed015c8610
# ╟─cda4f21e-44a5-43d7-9052-5d7be025d2cd
# ╠═77d5226d-0759-4133-b934-9155c4499b20
# ╟─b0aece7e-c7ef-404c-bab4-69b1cf85fb3e
# ╟─4681a0d4-81e5-42e1-98c4-2b587f438c7b
# ╟─cafb4dbc-6397-485e-a694-a018359ca594
# ╟─24e8ebf2-43a3-42d7-a4d9-6938e4804caa
# ╟─6bf1d725-d99c-4871-8c16-cc258839bd04
# ╟─b001acee-0b60-4904-b62b-c9e5a7b4d74f
# ╟─79730297-d61f-4bbb-b8ef-934862d3b197
# ╟─30994f1e-eaf3-427d-87ab-fc5a7927b0cc
# ╟─85d64f14-476c-408b-8080-8f1782ccc1b4
# ╟─2da5246e-7a7b-4058-9a7c-ca90dc176c9f
# ╟─c43c1615-7e09-432f-b373-d0daf80eb434
# ╟─63fdefda-00fc-46e8-b85c-30be7d662f42
# ╟─cbc2edab-7d77-4c87-8e57-82a0099fc34e
# ╟─2443a333-e6e6-4550-8a62-6c7a546d3380
# ╟─65db8d3c-a231-44bc-a5b9-b3d09619f272
# ╟─56a2e8cb-eee7-4427-a9c2-a53baac7b794
# ╠═c13826e8-2230-4d06-90f7-769d95360451
# ╟─bff76e5a-c9d5-4051-8ffc-e0ddb7423a5b
# ╟─4f6da2c6-ffc3-4684-8b32-1f576fa785c0
# ╟─c4eb217b-81d7-4128-8e21-935475f72214
# ╟─08cc6777-c9b0-410b-bc55-cceb99015cb9
# ╟─f3e6e227-9849-41fd-b99f-3c95868b4ad2
# ╟─f6f691e2-14c3-4483-b822-ee3a746271bc
# ╟─477c8a45-063b-4e96-8a60-db99dee9ec5d
# ╟─873f7287-5a3c-4253-8979-71e4bb1fe9a0
# ╟─8901d2db-09a8-4a5b-bd2e-8ee8428f4cb2
# ╟─bcc4a4b9-ee24-4982-bae3-37f39e54a3d1
# ╟─1d766cbb-82f5-4f36-b023-258823bd663b
# ╟─39a8c949-a9fc-4b91-91c0-f0d772484c50
# ╟─00a88397-7168-47bf-a53d-794577dd490f
# ╟─767f035d-fa4b-4099-9e37-cd85e9a0d83f
# ╟─77d700c2-555a-4793-9920-4e0778722fe7
# ╟─78cfc659-4925-42d5-9e25-4250cb956d3c
# ╟─6ba89644-0486-4334-bb1f-1907e69ec927
# ╟─97ef8fea-d22a-4929-a975-a69690cdfac5
# ╟─68ad58b0-c6e4-4609-b29a-fe594b37e6f9
# ╟─c44c1a73-3ce3-48ca-9108-4657654405a5
# ╟─bfc8ba94-4b00-4440-b3c6-3f322749f6c8
# ╟─9b0bb6c2-7df2-4030-9039-a1e65af46825
# ╟─683328c8-40f1-4b4b-bc99-9ea316fb51be
# ╟─2a194322-6246-4ede-9e41-30fffaa7765b
# ╟─5feec8d9-c530-474d-8c42-e1949f439cfb
# ╟─289db6e8-79e5-4ef5-945f-ba29b8e0a5d9
# ╠═56764702-3b30-47b6-916f-f62ec29b4d16
# ╟─5c23113e-f136-486c-8735-2aeeb178ade2
# ╠═1071b333-dd21-474c-8378-ae5517fa4e2d
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
