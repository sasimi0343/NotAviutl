local figurelist = {
	function(a)
		obj.load("figure", "éläpå`", color, a)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a, a / 4)
	end,
	function(a)
		obj.load("figure", "â~", color, a)
	end,
	function(a)
		obj.load("figure", "â~", color, a, a / 4)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a / 2)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a, a / 4)
		obj.draw()
		obj.load("figure", "éläpå`", color, a / 3)
	end,
	function(a, index)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * -2)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * 2)
		
		obj.load("figure", "éläpå`", color, 0)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * -2, a / 3 * -2)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * 2, a / 3 * -2)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * -2, a / 3 * 2)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3 * 2, a / 3 * 2)
		
		obj.load("figure", "éläpå`", color, 0)
	end,
	function(a, index)
		obj.load("figure", "â~", color, a * 2)
		obj.draw(a / 2, a / 2)
		obj.load("figure", "éläpå`", color, 0)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a)
		obj.effect("éŒÇﬂÉNÉäÉbÉsÉìÉO", "äpìx", 45, "ïù", a / 4)
	end,
	function(a)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / -3, a / 3 * 2)
		obj.load("figure", "éläpå`", color, a)
		obj.draw(a / 3, a / -3 * 2)
		
		obj.load("figure", "éläpå`", color, 0)
	end,
}

return figurelist