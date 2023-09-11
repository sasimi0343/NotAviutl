--[[

Function (obj, data)

obj - "obj" in Aviutl
time - Execute Duration
delay - Execute delay
data - data

]]

local E = require("easing")
local ft = {
	E.linear				,--  1
	E.inQuad				,--  2
	E.outQuad				,--  3
	E.inOutQuad				,--  4
	E.outInQuad				,--  5
	E.inCubic 				,--  6
	E.outCubic				,--  7
	E.inOutCubic			,--  8
	E.outInCubic			,--  9
	E.inQuart				,-- 10
	E.outQuart				,-- 11
	E.inOutQuart			,-- 12
	E.outInQuart			,-- 13
	E.inQuint				,-- 14
	E.outQuint				,-- 15
	E.inOutQuint			,-- 16
	E.outInQuint			,-- 17
	E.inSine				,-- 18
	E.outSine				,-- 19
	E.inOutSine				,-- 20
	E.outInSine				,-- 21
	E.inExpo				,-- 22
	E.outExpo				,-- 23
	E.inOutExpo				,-- 24
	E.outInExpo				,-- 25
	E.inCirc				,-- 26
	E.outCirc				,-- 27
	E.inOutCirc				,-- 28
	E.outInCirc				,-- 29
	E.inElastic				,-- 30
	E.outElastic			,-- 31
	E.inOutElastic			,-- 32
	E.outInElastic			,-- 33
	E.inBack				,-- 34
	E.outBack				,-- 35
	E.inOutBack				,-- 36
	E.outInBack				,-- 37
	E.inBounce				,-- 38
	E.outBounce				,-- 39
	E.inOutBounce			,-- 40
	E.outInBounce			,-- 41
}

local function easing_a(et, t, b, c, d, s, a, p)
	local r
	if( et >= 30 and et <= 33 ) then
		if( a ~= nil ) then
			a = a + c
		end
		r = ft[et](t,b,c,d,a,p)
	else
		r = ft[et](t,b,c,d,s)
	end
	
	if( r~=r ) then
		return b
	else
		return r
	end
end

function easing_b(par, st, ed, ratio)
	local module_name = "curve_editor"
	if not package.loaded[module_name] then
		package.preload[module_name] = package.loadlib(module_name .. ".auf", "luaopen_" .. module_name)
		require(module_name)
		package.preload[module_name] = nil
	end
	return curve_editor.getcurve(1, par, ratio, st, ed)
end

local function timeStarted(obj, delay, gld)
	if (gld == nil) then gld = 0 end
	if (obj.frame >= obj.index * delay + gld) then
		return true
	end
	return false
end

local function timeFinished(obj, time, delay, gld)
	if (gld == nil) then gld = 0 end
	if (time < 0) then
		return false
	end
	if ((obj.frame+1) > (obj.index * delay) + time + gld) then
		return true
	end
	return false
end

local function timeIn(o,t,d,gd)
	if (gd == nil) then gd = 0 end
	return timeStarted(o,d,gd) and (not timeFinished(o,t,d,gd))
end

local function timeCalc(o,t,d,gld)
	if (gld == nil) then gld = 0 end
	if (timeFinished(o,t,d,gld)) then
		return t
	else
		if (timeStarted(o,d,gld)) then
			return o.frame - (o.index * d) - gld
		else
			return 0
		end
	end
end

local function isValid(a)
	return not (a == nil)
end

--es is Non-negative: Curve Editor
--es is Negative: Easing_Track
local function easing(es,o,t,d,from,to, gld)
	if (gld == nil) then gld = 0 end
	if (es < 0) then
		local ea = math.abs(es)
		return easing_a(ea,timeCalc(o,t,d,gld),from,to-from,t)
	else
		return easing_b(es,from,to,(timeCalc(o,t,d,gld)/t))
	end
end

local function Blink(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			local t = timeCalc(obj, time, delay, gld)

			local freq = 1
			local tim = 1
			local alpha = 0
	
			if (isValid(data.freq)) then
				freq = data.freq
			end
			if (isValid(data.alpha)) then
				alpha = data.alpha
			end
			if (isValid(data.tim)) then
				tim = math.min(data.tim, freq)
			end

			if (freq == 0) then
				tim = math.max(tim, 1)
				if (rand(0,1,obj.index,math.floor(t/tim)) == 1) then
					obj.alpha = alpha
				end
			else
				if (t%(freq*2) >= ((freq*2)-tim)) then
					obj.alpha = alpha
				end
			end
		end
		--debug_print(tostring(timeCalc(obj, time, delay, gld)))
		if (isValid(data.dbg) and data.dbg == 1) then
			obj.draw()
			--debug_print(tostring(timeCalc(obj, time, delay)))
			obj.load("テキスト", tostring(timeCalc(obj, time, delay, gld)))
		end
	end
end

local function Raster(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			local t = timeCalc(obj, time, delay, gld)

			local width = 100
			local height = 100
			local freq = 0
			local isrand = 0
			local isvert = 0

			if (isValid(data.width)) then
				width = data.width
			end
			if (isValid(data.height)) then
				height = data.height
			end
			if (isValid(data.freq)) then
				freq = data.freq
			end
			if (isValid(data.isrand)) then
				isrand = data.isrand
			end
			if (isValid(data.isvert)) then
				isvert = data.isvert
			end

			obj.effect("ラスター", "横幅", width, "高さ", height, "周期", freq, "縦ラスター", isvert, "ランダム振幅", isrand)
		end
	end
end

local function Move(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			--local t = timeCalc(obj, time, delay, gld)

			local sx = 0
			local sy = 0
			local sz = 0
			local ex = 0
			local ey = 0
			local ez = 0
			local eas = -1
			local revmode = 0

			if (isValid(data.sx)) then
				sx = data.sx
			end
			if (isValid(data.sy)) then
				sy = data.sy
			end
			if (isValid(data.sz)) then
				sz = data.sz
			end
			if (isValid(data.ex)) then
				ex = data.ex
			end
			if (isValid(data.ey)) then
				ey = data.ey
			end
			if (isValid(data.ez)) then
				ez = data.ez
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.revmode)) then
				revmode = data.revmode
			end
			
			if (revmode == 1) then
				if (obj.index%2 == 1) then
					sx = -sx
					sy = -sy
					sz = -sz
				end
			elseif (revmode == 1) then
				if (obj.index%2 == 0) then
					sx = -sx
					sy = -sy
					sz = -sz
				end
			end
			
			obj.cx = obj.cx + easing(eas, obj, time, delay, sx, ex, gld)
			obj.cy = obj.cy + easing(eas, obj, time, delay, sy, ey, gld)
			obj.cz = obj.cz + easing(eas, obj, time, delay, sz, ez, gld)
		end
	end
end

local function RandomMove90Deg(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			local range = 100
			local eas = -1
			local seed = 1

			if (isValid(data.range)) then
				range = data.range
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.seed)) then
				seed = data.seed
			end

			local dir = rand(0,3,seed * (obj.index+1),1)
			if (dir == 0) then
				obj.cx = obj.cx + easing(eas, obj, time, delay, range, 0, gld)
			elseif (dir == 1) then
				obj.cx = obj.cx + easing(eas, obj, time, delay, -range, 0, gld)
			elseif (dir == 2) then
				obj.cy = obj.cy + easing(eas, obj, time, delay, range, 0, gld)
			elseif (dir == 3) then
				obj.cy = obj.cy + easing(eas, obj, time, delay, -range, 0, gld)
			end
		end
	end
end

local function RandomPos(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			
			local range = 100
			local seed = 1
			local rangeChange = -100
			local eas = -1
			local dir = 0
			if (isValid(data.range)) then
				range = data.range
			end
			if (isValid(data.seed)) then
				seed = data.seed
			end
			if (isValid(data.rangeChange)) then
				rangeChange = data.rangeChange
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.dir)) then
				dir = data.dir
			end

			local dirX = rand(0,100,seed * (obj.index+1),1)/100
			local dirY = rand(0,100,seed * (obj.index+1),2)/100

			local wari = math.max(dirX, dirY)
			dirX = ((dirX / wari)*2)-1
			dirY = ((dirY / wari)*2)-1

			if (dir == 1) then
				dirY = 0
			elseif (dir == 2) then
				dirX = 0
			end

			local rg = easing(eas, obj, time, delay, range, rangeChange, gld)

			obj.cx = obj.cx + (dirX * rg)
			obj.cy = obj.cy + (dirY * rg)
		end
	end
end

local function Zoom(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			
			local zoom = 100
			local seed = 1
			local zoomChange = -100
			local eas = -1
			if (isValid(data.zoom)) then
				zoom = data.zoom
			end
			if (isValid(data.seed)) then
				seed = data.seed
			end
			if (isValid(data.zoomchange)) then
				zoomChange = data.zoomchange
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end

			local rg = easing(eas, obj, time, delay, zoom, zoomChange, gld)
			
			obj.zoom = obj.zoom * (rg/100)
		end
	end
end



local function Rotate(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			--local t = timeCalc(obj, time, delay, gld)

			local sx = 0
			local sy = 0
			local sz = 0
			local ex = 0
			local ey = 0
			local ez = 0
			local eas = -1
			local revmode = 0

			if (isValid(data.sx)) then
				sx = data.sx
			end
			if (isValid(data.sy)) then
				sy = data.sy
			end
			if (isValid(data.sz)) then
				sz = data.sz
			end
			if (isValid(data.ex)) then
				ex = data.ex
			end
			if (isValid(data.ey)) then
				ey = data.ey
			end
			if (isValid(data.ez)) then
				ez = data.ez
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.revmode)) then
				revmode = data.revmode
			end
			
			if (revmode == 1) then
				if (obj.index%2 == 1) then
					sx = -sx
					sy = -sy
					sz = -sz
				end
			elseif (revmode == 1) then
				if (obj.index%2 == 0) then
					sx = -sx
					sy = -sy
					sz = -sz
				end
			end
			
			obj.rx = obj.rx + easing(eas, obj, time, delay, sx, ex, gld)
			obj.ry = obj.ry + easing(eas, obj, time, delay, sy, ey, gld)
			obj.rz = obj.rz + easing(eas, obj, time, delay, sz, ez, gld)
		end
	end
end



local function FanClipping(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			--local t = timeCalc(obj, time, delay, gld)

			local sx = 0
			local sy = 0
			local sa = 0
			local ex = 0
			local ey = 0
			local ea = 0
			local eas = -1
			local revmode = 0

			if (isValid(data.sx)) then
				sx = data.sx
			end
			if (isValid(data.sy)) then
				sy = data.sy
			end
			if (isValid(data.sa)) then
				sa = data.sa
			end
			if (isValid(data.ex)) then
				ex = data.ex
			end
			if (isValid(data.ey)) then
				ey = data.ey
			end
			if (isValid(data.ea)) then
				ea = data.ea
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.revmode)) then
				revmode = data.revmode
			end
			
			if (revmode == 1) then
				if (obj.index%2 == 1) then
					sx = -sx
					sy = -sy
					sa = -sa
				end
			elseif (revmode == 1) then
				if (obj.index%2 == 0) then
					sx = -sx
					sy = -sy
					sa = -sa
				end
			end
			
			local e_angle = easing(eas, obj, time, delay, sa, ea, gld)
			local e_x = easing(eas, obj, time, delay, sx, ex, gld)
			local e_y = easing(eas, obj, time, delay, sy, ey, gld)
			
			--関数化呼び出し
			require("fan_clipping")
			--関数実行
			fan_clipping(0,obj.track0,e_angle,e_x,e_y)
		end
	end
end

local function SquareClip(obj, time, delay, gld, data)
	if (isValid(data)) then
		if (timeIn(obj, time, delay, gld)) then
			local t = timeCalc(obj, time, delay, gld)/time

			local st = 0
			local ed = 0
			local seed = 0
			local hide = false
			local eas = -1
			local eas2 = -1
			local color = 0xffffff
			
			local x = obj.ox - obj.cx
			local y = obj.oy - obj.cy
			local z = obj.oz - obj.cz
			local r = obj.rz
			local rx = obj.rx
			local ry = obj.ry
			local alpha = obj.alpha
			local zoom = obj.zoom
			
			if (isValid(data.st)) then
				st = data.st
			end
			if (isValid(data.ed)) then
				ed = data.ed
			end
			if (isValid(data.seed)) then
				seed = data.seed
			end
			if (isValid(data.hide)) then
				hide = data.hide
			end
			if (isValid(data.eas)) then
				eas = data.eas
			end
			if (isValid(data.eas2)) then
				eas2 = data.eas2
			end
			if (isValid(data.color)) then
				color = data.color
			end
			
			obj.setoption("drawtarget","tempbuffer", obj.w, obj.h)
			if ((not hide) or (t >= 0.5)) then
				obj.ox = 0
				obj.oy = 0
				obj.oz = 0
				obj.rz = 0
				obj.rx = 0
				obj.ry = 0
				obj.zoom = 1
				obj.alpha = 1
				obj.draw()
			end
			
			obj.load("figure", "四角形", color, math.max(obj.w, obj.h))
			obj.aspect = ((obj.h / (obj.w + obj.h)) * 2) - 1
			
			local direction = {"上", "下", "左", "右"}
			local dir = direction[1]
			local siz = obj.w
			if (seed == -1) then
				dir = direction[1]
				siz = -obj.h
			elseif (seed == -2) then
				dir = direction[2]
				siz = obj.h
			elseif (seed == -3) then
				dir = direction[3]
				siz = -obj.w
			elseif (seed == -4) then
				dir = direction[4]
				siz = obj.w
			else
				local ra = rand(1, 4, seed, obj.index)
				dir = direction[ra]
				if (ra == 1 or ra == 2) then
					if (ra == 1) then siz = -obj.h else siz = obj.h end
				else
					if (ra == 3) then siz = -obj.w else siz = obj.w end
				end
			end
			
			if (t < 0.5) then
				local clip = easing(eas, obj, time, delay, st, ed - st, gld)
				
				local clippp = math.abs(siz) * (clip/100)
				
				obj.effect("クリッピング", dir, clippp, "中心の位置を変更", 0)
				
				if (dir == direction[1] or dir == direction[2]) then
					obj.draw(0, -(siz/2) * ((clip/200)+0.5))
				else
					obj.draw(-(siz/2) * ((clip/200)+0.5))
				end
			else
				local clip = easing(eas2, obj, time/2, delay, ed, st - ed, gld + (time/2))
				
				local clippp = math.abs(siz) * (clip/100)
				
				obj.effect("クリッピング", dir, clippp, "中心の位置を変更", 0)
				
				if (dir == direction[1] or dir == direction[2]) then
					obj.draw(0, (siz/2) * (clip/100))
				else
					obj.draw((siz/2) * (clip/100))
				end
			end
			
			obj.setoption("drawtarget","framebuffer")
			obj.load("tempbuffer")
			
			obj.ox = x
			obj.oy = y
			obj.oz = z
			obj.rz = r
			obj.rx = rx
			obj.ry = ry
			obj.zoom = zoom
			obj.alpha = alpha
		elseif (not timeStarted(obj, delay, gld)) then
			obj.alpha = 0
		end
	end
end

return {
	Blink = Blink,
	Raster = Raster,
	Move = Move,
	RandomMove90Deg = RandomMove90Deg,
	RandomPos = RandomPos,
	Zoom = Zoom,
	Rotate = Rotate,
	FanClipping = FanClipping,
	SquareClip = SquareClip
}