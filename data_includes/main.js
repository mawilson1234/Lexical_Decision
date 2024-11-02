// This is a PCIbex implementation of a simple Lexical Decision task for
// LINGUIST412 @ University of Massachusetts

// Brian Dillon, October 2021
// CC-BY

// Updated slightly by Michael Wilson November 2024

PennController.ResetPrefix(null) // Shorten command names (keep this)
DebugOff()

var centered_justified_style = {
	'text-align': 'justify', 
	margin: '0 auto', 
	'margin-bottom': '3em',
	width: '30em',
	'font-size': '16px',
	'font-family': 'Helvetica, sans-serif'
}

var trial_style = {
	'font-family': 'Helvetica, sans-serif',
	'font-size': '48px'
}

Sequence(
	'instructions',
	randomize('trial') ,
	SendResults(),
	'end'
)

newTrial('instructions',
	fullscreen(),
	
	newText(
		`<p>Welcome! In this experiment, we want you to decide as quickly as possible whether what you see is a word of English, or not.</p><p>
			If you think it IS a word, press the 'f' button.</p><p>
			If you think it IS NOT a word, press the 'j' button.</p><p>
			Try to respond as accurately and quickly as possible. If you wait more than 6 seconds, you will not be able to respond, and the experiment will move on.</p><p>
		`
	)
		.css(centered_justified_style)
		.print()		
	,
	
	newButton('Click when you are ready to begin')
		.css('font-family', 'Helvetica, sans-serif')
		.css('font-size', '16px')
		.center()
		.print()
		.wait()
).setOption('countsForProgressBar', false)

Template('stimuli.csv', currentrow => 
	newTrial(
		'trial',
		
		newText(`cross`, `+`)
			.css(trial_style)
			.print('center at 50%', 'middle at 50%')
		,

		newText(`Reminder: Press F it is a word, press J if it is not a word.`)
			.css(centered_justified_style)
			.print('center at 50%', 'bottom at 90%')
		,

		newTimer('wait1', 1000)
			.start()
			.wait()
		,
		
		getText(`cross`).remove(),
		
		newTimer('wait2', 500)
			.start()
			.wait()
		,
			
		newTimer('deadline', 6000)
			.start()
		,

		newVar('RT')
			.global()
			.set(v => Date.now())
		,

		newText(currentrow.WORD)
			.css(trial_style)
			.print('center at 50%', 'middle at 50%')
		,

		newKey('response', 'F', 'J')
			.log('first')
			.callback(getTimer('deadline').stop())
			.callback(getVar('RT').set(v => Date.now() - v))
		,
		
		getTimer('deadline')
			.wait()
	)
		.log('word',      currentrow.WORD)
		.log('frequency', currentrow.FREQ)
		.log('RT',        getVar('RT'))
)

newTrial('end',
	exitFullscreen()
	,
	
	newText('The is the end of the experiment, you can now close this window. Thank you!')
		.css(trial_style)
		.center()
		.print('center at 50%', 'bottom at 80%')
	,
	
	newButton()
		.wait()
)
.setOption('countsForProgressBar', false)