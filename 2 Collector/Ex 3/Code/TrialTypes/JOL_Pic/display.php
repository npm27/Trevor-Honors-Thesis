<?php
    if (!isset($text) || $text === '') { 
        $text = 'How likely are you to correctly recall the second item in the pair?|Type your response on a scale from 0-100 and then press Enter.';
    }

    $texts = explode('|', $text);
    $mainText = array_shift($texts);
?>

<div class="textcenter">
    <div><?php echo isset($text) ? $text : ""; ?></div>

<br>

<div class="pic2"><?php echo show($cue); ?></div>

<br>
  
<div class="textcenter">
    <input name="JOL" type="text" value="" autocomplete="off" class="forceNumeric textcenter collectorInput">
    <button class="collectorButton" id="FormSubmitButton">Submit</button>
</div>
