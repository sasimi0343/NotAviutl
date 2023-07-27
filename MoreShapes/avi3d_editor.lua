if (rikky_module == nil) then
	require("rikky_module")
end

UINUM = {}

function Slider(tbl, x, y, width, min, max, initial, per, label)
	local variable = tbl.value
	
	if (variable == nil) then variable = initial end
	
	local mx,my = rikky_module.mouse()
	--print((x - (width / 2)) .. ", " .. mx .. ", " .. (x + (width / 2)))
	--print((y - 30) .. ", " .. my .. ", " .. (y + 30))
	if ((mx >= x - (width / 2)) and (mx < x + (width / 2)) and (my >= y - 30) and (my < y + 30)) then
		variable = math.floor((min + (((mx - x + (width / 2)) / width) * (max - min))) * math.pow(10, per-1)) * math.pow(10, per-1)
		print(variable)
		if ((variable >= min) and (variable <= max)) then
			tbl.value = variable
		end
	end
	local drawX = ((((variable - min) / (max - min)) * width) - (width / 2))
	
	obj.setoption("drawtarget", "tempbuffer", width, 60)
	obj.load("figure", "四角形", 0x999999, 1)
	obj.effect("リサイズ", "X", width, "Y", 40, "ドット数でサイズ指定", 1, "補間なし", 1)
	obj.draw()
	
	obj.load("figure", "四角形", 0xdddddd, 1)
	obj.effect("リサイズ", "X", 30, "Y", 60, "ドット数でサイズ指定", 1, "補間なし", 1)
	obj.draw(drawX)
	
	obj.setfont("MS UI Gothic", 40, 0, 0xffffff)
	obj.setoption("blend", 11)
	
	if (not (label == nil)) then
		
		obj.load("text", label)
		
		obj.draw((obj.w - width) / 2)
		
	end
	
	obj.load("text", tostring(variable))
	obj.draw((width - obj.w) / 2)
	
	obj.setoption("drawtarget", "framebuffer")
	obj.setoption("blend", 0)
	obj.load("tempbuffer")
	
	table.insert(UINUM, {
	id = #UINUM+1,
	ox = x,
	oy = y,
	rz = 0
	})
	rikky_module.image("w", "a3e_UI" .. tostring(#UINUM))
	
	obj.alpha = 0
	
	--obj.draw(-obj.x, -obj.y, -obj.z, 1, 1, 0, 0, 0)
end

function PostRender()
	obj.load("figure", "四角形", 0x999999, 1)
	for k,v in pairs(UINUM) do
		rikky_module.image("r", "a3e_UI" .. tostring(v.id))
		obj.draw(v.ox-obj.x, v.oy-obj.y, -obj.z, 1, 1, 0, 0, v.rz)
	end
end

function ResetUI()
	UINUM = {}
end

function DrawAllPoints(tbl)
	obj.load("figure", "円", 0x121212, 10)
	for k,v in pairs(tbl) do
		local p1 = v[1]
		local p2 = v[2]
		local p3 = v[3]
		local p4 = v[4]
		
		obj.draw(p1.x, p1.y, p1.z)
		obj.draw(p2.x, p2.y, p2.z)
		obj.draw(p3.x, p3.y, p3.z)
		obj.draw(p4.x, p4.y, p4.z)
	end
end

return {
	Slider = Slider,
	PostRender = PostRender,
	UINUM = UINUM,
	ResetUI = ResetUI,
	DrawAllPoints = DrawAllPoints
}