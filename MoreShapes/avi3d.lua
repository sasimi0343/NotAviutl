require("getcolortools")
local gct = getcolortools

if (rikky_module == nil) then
	require("rikky_module")
end

geo_shading_cache = geo_shading_cache or {}

function pt(layer, index, index2)
	return layer .. "_" .. index .. "_" .. index2
end

function Draw(obj, tbl, lights, lightsinfo, geo, world)
	rikky_module.image("w", "avi3d_image")
	--obj.copybuffer("cache:avi3d_image", "obj")
	local index = 0
	for k,v in pairs(tbl) do
		--obj.load("figure", "四角形", 0xffffff, 1)
		--obj.copybuffer("obj", "cache:avi3d_image")
		rikky_module.image("r", "avi3d_image")
		
		if (not (lightsinfo == nil)) then
			if (not (geo.shading == nil) and (not (geo.shading.style == nil)) and (not (geo.shading.style == 0))) then
				v[5] = 100
				local point = pt(geo.layer, geo.index, index)
				if (geo.shading.static == 1 and geo_shading_cache[point]) then
					rikky_module.image("r", "avi3d_lightcache_" .. point)
				else
					LightToTexture(lightsinfo, v, world, v)
				end
				if (geo.shading.static == 1 and (not geo_shading_cache[point])) then
					rikky_module.image("w", "avi3d_lightcache_" .. point)
					geo.shading.rendered = true
					geo_shading_cache[point] = true
				end
			else
				v[5] = 0
				for _,light in pairs(lightsinfo) do
					v = Lighting(obj, v, light, geo)
				end
			end
			
		end
		
		if (lights) then obj.effect("色調補正", "明るさ", v[5]) end
		
		local p1 = v[1]
		local p2 = v[2]
		local p3 = v[3]
		local p4 = v[4]
		
		local uv = v[6]
		if (uv == nil) then
			UVValidate(obj, tbl)
			uv = v[6]
		end
		local uv0,uv1,uv2,uv3 = uv[1], uv[2], uv[3], uv[4]
		if (uv0 == nil) then uv0 = {x = 0, y = 0} end
		if (uv1 == nil) then uv1 = {x = obj.w, y = 0} end
		if (uv2 == nil) then uv2 = {x = obj.w, y = obj.h} end
		if (uv3 == nil) then uv3 = {x = 0, y = obj.h} end
		
		if (not (v[7] == nil)) then
			local emi = 0
			if (not (geo.shading == nil) and (not (geo.shading.emitting == nil)) and (not (geo.shading.emitting == 0))) then
				emi = math.min((geo.shading.emitting / 100), 1)
			end
			local mat = rikky_module.materialdrawEx({
				ambient = {R = 50, G = 50, B = 50},
				emissive = {R = 255 * emi, G = 255 * emi, B = 255 * emi},
				damping = 50,
				drawhq = true,
				drawhq_partition = 100,
				light = v[7]
			})
			
			mat:drawpoly(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z,
			uv0.x, uv0.y, uv1.x, uv1.y, uv2.x, uv2.y, uv3.x, uv3.y)
		else
		obj.drawpoly(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z, p3.x, p3.y, p3.z, p4.x, p4.y, p4.z,
		uv0.x, uv0.y, uv1.x, uv1.y, uv2.x, uv2.y, uv3.x, uv3.y, geo.alpha)
		end
		index = index + 1
	end
	
end

function UVValidate(obj, surs)
	for _,v in pairs(surs) do
		if (v[6] == nil) then v[6] = {{x = 0, y = 0}, {x = obj.w, y = 0}, {x = obj.w, y = obj.h}, {x = 0, y = obj.h}} end
	end
end

function Saibunka(obj, surs, vert, hori, hori2)
	UVValidate(obj, surs)
	local addsurs = {}
	for _,v in pairs(surs) do
		if ((not ((v[1].x == v[2].x and v[4].x == v[3].x) and (v[1].x == v[4].x and v[2].x == v[3].x))) and (not (hori == 0))) then
		local prevlx12, prevlx43 = v[1].x, v[4].x
		local uvoriginal = v[6]
		local topW = uvoriginal[2].x - uvoriginal[1].x
		local bottomW = uvoriginal[3].x - uvoriginal[4].x
		for x=1,hori do
			local dis12 = (v[2].x - v[1].x) / (hori+1)
			local dis43 = (v[3].x - v[4].x) / (hori+1)
			
			local lx12 = v[1].x + (x * (dis12))
			local lx43 = v[4].x + (x * (dis43))
			
			local prev1 = {x = prevlx12, y = v[1].y, z = v[1].z}
			local prev4 = {x = prevlx43, y = v[4].y, z = v[4].z}
			
			local subsur = {prev1, v[2], v[3], prev4, v[5]}
			
			local nv2 = {x = lx12, y = v[2].y, z = v[2].z}
			local nv3 = {x = lx43, y = v[3].y, z = v[3].z}
			
			subsur[2] = nv2
			subsur[3] = nv3
			
			local uv1 = {x = (uvoriginal[1].x + (((x-1) / (hori+1)) * topW)), y = uvoriginal[1].y}
			local uv2 = {x = (lx12 / (v[2].x - v[1].x)) * topW, y = uvoriginal[2].y}
			local uv3 = {x = (lx43 / (v[3].x - v[4].x)) * bottomW, y = uvoriginal[3].y}
			local uv4 = {x = (uvoriginal[4].x + (((x-1) / (hori+1)) * bottomW)), y = uvoriginal[4].y}
			
			subsur[6] = {uv1, uv2, uv3, uv4}
			
			prevlx12 = lx12
			prevlx43 = lx43
			
			table.insert(addsurs, subsur)
		end
		
		local prev1 = {x = prevlx12, y = v[1].y, z = v[1].z}
		local prev4 = {x = prevlx43, y = v[4].y, z = v[4].z}
		v[1] = prev1
		v[4] = prev4
		
		local uv1 = {x = (uvoriginal[1].x + ((hori / (hori+1)) * topW)), y = uvoriginal[1].y}
		local uv4 = {x = (uvoriginal[4].x + ((hori / (hori+1)) * bottomW)), y = uvoriginal[4].y}
		v[6] = {uv1, uvoriginal[2], uvoriginal[3], uv4}
		end
	end
	for k,v in pairs(addsurs) do
		table.insert(surs, v)
	end
	
	
	addsurs = {}
	for _,v in pairs(surs) do
		if ((not ((v[1].y == v[4].y and v[2].y == v[3].y) and (v[1].y == v[2].y and v[3].y == v[4].y))) and (not (vert == 0))) then
		local prevly14, prevly23 = v[1].y, v[2].y
		local uvoriginal = v[6]
		local leftH = uvoriginal[4].y - uvoriginal[1].y
		local rightH = uvoriginal[3].y - uvoriginal[2].y
		for y=1,vert do
			local dis14 = (v[4].y - v[1].y) / (vert+1)
			local dis23 = (v[3].y - v[2].y) / (vert+1)
			
			local lx14 = v[1].y + (y * (dis14))
			local lx23 = v[2].y + (y * (dis23))
			
			local prev1 = {x = v[1].x, y = prevly14, z = v[1].z}
			local prev2 = {x = v[2].x, y = prevly23, z = v[2].z}
			
			local subsur = {prev1, prev2, v[3], v[4], v[5]}
			
			local nv4 = {x = v[4].x, y = lx14, z = v[4].z}
			local nv3 = {x = v[3].x, y = lx23, z = v[3].z}
			
			subsur[3] = nv3
			subsur[4] = nv4
			
			local uv1 = {y = (uvoriginal[1].y + (((y-1) / (vert+1)) * leftH)), x = uvoriginal[1].x}
			local uv2 = {y = (uvoriginal[2].y + (((y-1) / (vert+1)) * rightH)), x = uvoriginal[1].x}
			local uv3 = {y = (lx14 / (v[4].y - v[1].y)) * rightH, x = uvoriginal[3].x}
			local uv4 = {y = (lx23 / (v[3].y - v[2].y)) * leftH, x = uvoriginal[4].x}
			
			subsur[6] = {uv1, uv2, uv3, uv4}
			
			prevly14 = lx14
			prevly23 = lx23
			
			table.insert(addsurs, subsur)
		end
		
		local prev1 = {x = v[1].x, y = prevly14, z = v[1].z}
		local prev2 = {x = v[2].x, y = prevly23, z = v[2].z}
		v[1] = prev1
		v[2] = prev2
		
		end
	end
	for k,v in pairs(addsurs) do
		table.insert(surs, v)
	end
	
	
	addsurs = {}
	for _,v in pairs(surs) do
		if ((not ((v[1].z == v[2].z and v[4].z == v[3].z) and (v[1].z == v[4].z and v[2].z == v[3].z))) and (not (hori2 == 0))) then
		local prevlx12, prevlx43 = v[1].z, v[4].z
		for z=1,hori2 do
			local dis12 = (v[2].z - v[1].z) / (hori2+1)
			local dis43 = (v[3].z - v[4].z) / (hori2+1)
			
			local lx12 = v[1].z + (z * (dis12))
			local lx43 = v[4].z + (z * (dis43))
			
			local prev1 = {z = prevlx12, y = v[1].y, x = v[1].x}
			local prev4 = {z = prevlx43, y = v[4].y, x = v[4].x}
			
			local subsur = {prev1, v[2], v[3], prev4, v[5]}
			
			local nv2 = {z = lx12, y = v[2].y, x = v[2].x}
			local nv3 = {z = lx43, y = v[3].y, x = v[3].x}
			
			subsur[2] = nv2
			subsur[3] = nv3
			
			prevlx12 = lx12
			prevlx43 = lx43
			
			table.insert(addsurs, subsur)
		end
		
		local prev1 = {z = prevlx12, y = v[1].y, x = v[1].x}
		local prev4 = {z = prevlx43, y = v[4].y, x = v[4].x}
		v[1] = prev1
		v[4] = prev4
		end
	end
	for k,v in pairs(addsurs) do
		table.insert(surs, v)
	end
end

function SetPosition(obj, geos, tbl1, tbl2, x, y, z, samecheck)
	for i=1,math.min(#tbl1, #tbl2) do
		local po = GetPoint(geos, tbl1[i], tbl2[i], samecheck)
		for k,v in pairs(po) do
			if (not (v == nil)) then
				v.x = v.x + x
				v.y = v.y + y
				v.z = v.z + z
			end
		end
	end
end

function GetPoint(geo, indexA, indexB, samecheck)
	local points = {}
	local i = 0
	for _,v in pairs(geo) do
		local br2 = false
		if (i == indexA) then
			local l = 0
			for _,v2 in pairs(v.surface) do
				local br1 = false
				for j=1,4 do
					if (indexB == l) then
						table.insert(points, v2[j])
						if (not samecheck) then return points end
						br1 = true
						break
					end
					l = l + 1
				end
				if (br1) then
					br2 = true
					break
				end
			end
		end
		i = i + 1
		if (br2) then
			break
		end
	end
	
	if (not (points[1] == nil)) then
	local tp = points[1]
	for _,v in pairs(geo) do
		if (i == indexA) then
			for _,v2 in pairs(v.surface) do
				for j=1,3 do
					if (((v2[j].x == tp.x) and (v2[j].y == tp.y) and (v2[j].y == tp.y)) and (not tp == v2[j])) then
						table.insert(points, v2[j])
					end
				end
			end
		end
		i = i + 1
	end
	
	end
	
	return points
end

function Lighting(obj, surface, light, geo)
	
	if (not (geo.shading == nil) and (not (geo.shading.smooth == nil)) and (not (geo.shading.smooth == 0))) then
		surface[5] = 100
		if (surface[7] == nil) then surface[7] = {} end
		table.insert(surface[7], LightToMaterialEx(light))
	else
		local hx,hy,hz = 0,0,0
		hx = surface[1].x + surface[2].x + surface[3].x + surface[4].x
		hy = surface[1].y + surface[2].y + surface[3].y + surface[4].y
		hz = surface[1].z + surface[2].z + surface[3].z + surface[4].z
		hx = hx / 4
		hy = hy / 4
		hz = hz / 4
		
		hx,hy,hz = gct.Rot_rpy({hx, hy, hz}, math.rad(obj.rz), math.rad(obj.ry), math.rad(obj.rx))
		
		local dx,dy,dz = obj.x+obj.ox+hx,obj.y+obj.oy+hy,obj.z+obj.oz+hz
		local sx,sy,sz = math.abs(dx - light.x),math.abs(dy - light.y),math.abs(dz - light.z)
		
		local d_xy = ((sx * sx) + (sy * sy))
		local d_yz = ((sy * sy) + (sz * sz))
		local d_zx = ((sz * sz) + (sx * sx))
		
		local distance = math.sqrt(d_zx + (sy * sy))
		
		local range = math.max(light.range - distance, 0)
		local intensity = light.intensity * math.pow((range / light.range), light.contrast)
		
		--print(intensity)
		surface[5] = surface[5] + intensity
	end
	
	return surface
end

function PointRotate(p, rx, ry, rz)
return {gct.Rot_rpy(PointToArray(p), math.rad(rz), math.rad(ry), math.rad(rx))}
end

function GlobalPoint(p, geo)
local p1 = PointRotate(p, geo.rx, geo.ry, geo.rz)
return {p1[1] + geo.ox, p1[2] + geo.oy, p1[3] + geo.oz}
end

function LightPixel(hx, hy, hz, lights, world, surface)
	hx,hy,hz = gct.Rot_rpy({hx, hy, hz}, math.rad(obj.rz), math.rad(obj.ry), math.rad(obj.rx))
	
	local dx,dy,dz = obj.x+obj.ox+hx,obj.y+obj.oy+hy,obj.z+obj.oz+hz
	local intensity = 0
	
	for k,v in pairs(lights) do
		local sx,sy,sz = math.abs(dx - v.x),math.abs(dy - v.y),math.abs(dz - v.z)
		
		--local d_xy = ((sx * sx) + (sy * sy))
		--local d_yz = ((sy * sy) + (sz * sz))
		local d_zx = ((sz * sz) + (sx * sx))
		
		local distance = math.sqrt(d_zx + (sy * sy))
		
		local range = math.max(v.range - distance, 0)
		local inte = (v.intensity/100) * math.pow((range / v.range), v.contrast)
		
		intensity = intensity + (math.max(1 - intensity, 0) * (inte))
		
		if (world.shadow) then
			intensity = (1 - ShadowPixel(hx, hy, hz, dx, dy, dz, v, world, surface)) * intensity
		end
	end
	--print(intensity)
	
	return intensity
end

function Distance(posA, posB)
	local xA,yA,zA = posA[1],posA[2],posA[3]
	local xB,yB,zB = posB[1],posB[2],posB[3]
	
	local sx,sy,sz = xA - xB,yA - yB,zA - zB
	--local d_xy = ((sx * sx) + (sy * sy))
	--local d_yz = ((sy * sy) + (sz * sz))
	local d_zx = ((sz * sz) + (sx * sx))
	local distance = math.sqrt(d_zx + (sy * sy))
	
	return distance
end

function Normalize(pos, distance)
	local x,y,z = pos[1],pos[2],pos[3]
	x = x / distance
	y = y / distance
	z = z / distance
	return {x, y, z}
end

function InBox(pos8, pos)
	local pos1 = pos8[1]
	local pos2 = pos8[2]
	local pos3 = pos8[3]
	local pos4 = pos8[4]
	local pos5 = pos8[5]
	local pos6 = pos8[6]
	local pos7 = pos8[7]
	local pos8 = pos8[8]
	
	--どう見たって入っていない時
	if (pos.x > math.max(pos1.x, pos2.x, pos3.x, pos4.x, pos5.x, pos6.x, pos7.x, pos8.x)) then
		return false
	elseif (pos.x < math.min(pos1.x, pos2.x, pos3.x, pos4.x, pos5.x, pos6.x, pos7.x, pos8.x)) then
		return false
	end
	
	if (pos.y > math.max(pos1.y, pos2.y, pos3.y, pos4.y, pos5.y, pos6.y, pos7.y, pos8.y)) then
		return false
	elseif (pos.y < math.min(pos1.y, pos2.y, pos3.y, pos4.y, pos5.y, pos6.y, pos7.y, pos8.y)) then
		return false
	end
	
	if (pos.z > math.max(pos1.z, pos2.z, pos3.z, pos4.z, pos5.z, pos6.z, pos7.z, pos8.z)) then
		return false
	elseif (pos.z < math.min(pos1.z, pos2.z, pos3.z, pos4.z, pos5.z, pos6.z, pos7.z, pos8.z)) then
		return false
	end
	
	
	--X
	local x = pos.x
	local y = pos.y
	local z = pos.z
	local xd1Max = pos1.x - pos2.x
	local yd1Max = pos2.y + ((pos1.y - pos2.y) * ((x - pos2.x) / xd1Max))
	local zd1Max = pos2.z + ((pos1.z - pos2.z) * ((x - pos2.x) / xd1Max))
	
	local xd2Max = pos3.x - pos4.x
	local yd2Max = pos4.y + ((pos3.y - pos4.y) * ((x - pos4.x) / xd2Max))
	local zd2Max = pos4.z + ((pos3.z - pos4.z) * ((x - pos4.x) / xd2Max))
	
	if (
	((y >= yd1Max and y <= yd2Max) or (y >= yd2Max and y <= yd1Max)) and
	((z >= zd1Max and z <= zd2Max) or (z >= zd2Max and y <= zd1Max))
	) then
		return true
	end
	
	local xd3Max = pos5.x - pos6.x
	local yd3Max = pos6.y + ((pos5.y - pos6.y) * ((x - pos6.x) / xd3Max))
	local zd3Max = pos6.z + ((pos5.z - pos6.z) * ((x - pos6.x) / xd3Max))
	
	local xd4Max = pos7.x - pos8.x
	local yd4Max = pos8.y + ((pos7.y - pos8.y) * ((x - pos8.x) / xd4Max))
	local zd4Max = pos8.z + ((pos7.z - pos8.z) * ((x - pos8.x) / xd4Max))
	
	if (
	((y >= yd3Max and y <= yd4Max) or (y >= yd4Max and y <= yd3Max)) and
	((z >= zd3Max and z <= zd4Max) or (z >= zd4Max and y <= zd3Max))
	) then
		return true
	end
	
	local ydd1Max = yd1Max
	local ydd2Max = yd3Max
	local ydd1Min = yd2Max
	local ydd2Min = yd4Max
	
	local zdd1Max = zd1Max
	local zdd2Max = zd3Max
	local zdd1Min = zd2Max
	local zdd2Min = zd4Max
	
	local ydddMax = ydd2Max + ((ydd1Max - ydd2Max) * ((z - zdd2Max) / (zdd1Max - zdd2Max)))
	local zdddMax = zdd2Max + ((zdd1Max - zdd2Max) * ((y - ydd2Max) / (ydd1Max - ydd2Max)))
	local ydddMin = ydd2Min + ((ydd1Min - ydd2Min) * ((z - zdd2Min) / (zdd1Min - zdd2Min)))
	local zdddMin = zdd2Min + ((zdd1Min - zdd2Min) * ((y - ydd2Min) / (ydd1Min - ydd2Min)))
	
	if (
	((y >= ydddMin and y <= ydddMax) or (y >= ydddMax and y <= ydddMin)) and
	((z >= zdddMin and z <= zdddMax) or (z >= zdddMax and y <= zdddMin))
	) then
		return true
	end
	
	return false
end

function PointToArray(p)
	return {p.x, p.y, p.z}
end
function ArrayToPoint(p)
	return {x = p[1], y = p[2], z = p[3]}
end

function NormalizedVector(p1, p2, p3)
	local p21 = ArrayToPoint({SubtractPosition(p2, p1)})
	local p31 = ArrayToPoint({SubtractPosition(p3, p1)})
	
	--local x,y,z = AddPosition(p21, p31)
	gct.Vector.Cross(PointToArray(p21), PointToArray(p31))
end

function PointToString(pos)
	return pos[1] .. ", " .. pos[2] .. ", " .. pos[3]
end

function NotNormalized(a,b,c)
	local AB=gct.Vector.Sub(b,a)
	local BC=gct.Vector.Sub(c,b)
	local N = gct.Vector.Cross(AB,BC)
	return N
end

function ScalePoint(p, scale)
	return {p[1]*scale, p[2]*scale, p[3]*scale}
end

function SubPoint(p1, p2)
	return {p1[1]-p2[1],p1[2]-p2[2],p1[3]-p2[3]}
end
function AddPoint(p1, p2)
	return {p1[1]+p2[1],p1[2]+p2[2],p1[3]+p2[3]}
end

function ShadowPixel(hx,hy,hz, dx, dy, dz, light, world, surface)
	local shadow = 0
	--local lights = world.lights
	local surs = world.surface_all
	
	local sx,sy,sz = dx - light.x,dy - light.y,dz - light.z --l, m, n
	local d_zx = ((sz * sz) + (sx * sx))
	local distance = math.sqrt(d_zx + (sy * sy))
	if (light.range < distance) then
		return 0
	end
	for _,geos in pairs(world.objs) do
		for _,geo in pairs(geos.geometory) do
			for k,v in pairs(geo.surface) do
				if (not (v == surface)) then
					
					local p1,p2,p3,p4 = GlobalPoint(v[1], geo),GlobalPoint(v[2], geo),GlobalPoint(v[3], geo),GlobalPoint(v[4], geo)
					
					local S1 = gct.Vector.Norm_surface(p1, p2, p3)
					local S2 = gct.Vector.Norm_surface(p3, p4, p1)
					
					local d1 = - ((S1[1] * p1[1]) + (S1[2] * p1[2]) + (S1[3] * p1[3]))
					local d2 = - ((S2[1] * p1[1]) + (S2[2] * p1[2]) + (S2[3] * p1[3]))
					
					local t1 = ((S1[1] * dx) + (S1[2] * dy) + (S1[3] * dz) + d1) / ((S1[1] * sx) + (S1[2] * sy) + (S1[3] * sz))
					local t2 = ((S1[1] * dx) + (S1[2] * dy) + (S1[3] * dz) + d2) / ((S1[1] * sx) + (S1[2] * sy) + (S1[3] * sz))
					
					local intersection1 = {(sx * t1) + dx, (sy * t1) + dy, (sz * t1) + dz}
					local intersection2 = {(sx * t2) + dx, (sy * t2) + dy, (sz * t2) + dz}
					
					local CP_d = Distance(intersection1, p1)
					local CP2_d = Distance(intersection2, p1)
					if (CP_d < distance or CP2_d < distance) then
						local CP = SubPoint(intersection1, p1)
						local CP2 = SubPoint(intersection2, p1)
						
						
						if (((CP[1] * sx) >= 0 or (CP[2] * sy) >= 0 or (CP[3] * sz) >= 0) or ((CP2[1] * sx) >= 0 or (CP2[2] * sy) >= 0 or (CP2[3] * sz) >= 0)) then
							local CA,CB = SubPoint(p2, p1), SubPoint(p3, p1)
							local CD = SubPoint(p4, p1)
							
							local st1 = ((CP[1] * CA[2]) - (CP[2] * CA[1])) / (CB[1] - (CA[1] * CB[2]))
							local st2 = ((CP2[1] * CB[2]) - (CP2[2] * CB[1])) / (CD[1] - (CB[1] * CD[2]))
							
							local s1 = (CP[2] - (st1 * CB[2])) / CA[2]
							local s2 = (CP2[2] - (st2 * CD[2])) / CB[2]
							if (s1 >= 0 and st1 >= 0 and (s1 + st1) <= 1) then
								--gct.Draw_line3D({light.x - obj.ox, light.y - obj.oy, light.z - obj.oz}, SubPoint(intersection1, {obj.ox, obj.oy, obj.oz}), 0.5, 0xff0000)
								--gct.Draw_line3D(SubPoint(intersection1, {obj.ox, obj.oy, obj.oz}), {dx - obj.ox, dy - obj.oy, dz - obj.oz}, 0.5, 0x0000ff)
								return s1 + st1
							end
							
							if (s2 >= 0 and st2 >= 0 and (s2 + st2) <= 1) then
								return s2 + st2
							end
						end
					end
					
					
				end
			end
		end
	end
	return 0
end

function LightToMaterialEx(light)
	local inte = math.min((light.intensity / 100), 1)
	local deg = math.min((light.range / 18), 179.9)
	local spec = math.min((light.specular / 100), 1)
	local ltype = "spotlight"
	local ldouble = false
	if (light.type == "pointlight") then
		ltype = "spotlight"
		ldouble = true
	end
	
	local li = {
		color = {R = 255 * inte, G = 255 * inte, B = 255 * inte},
		specular = {R = 255 * spec, G = 255 * spec, B = 255 * spec, shininess = light.specular},
		position = {x = light.x, y = light.y, z = light.z},
	}
	
	if (light.type == "spotlight") then
		li.option = {
			type = ltype,
			nx = 0,
			ny = 1,
			nz = 0,
			degree = deg,
			double = ldouble,
			degree2 = deg,
			nx2 = 0,
			ny2 = 0,
			nz2 = 0,
		}
	end
	
	return li
end

function AddPosition(posA, posB)
	local x = posB.x + posA.x
	local y = posB.y + posA.y
	local z = posB.z + posA.z
	
	return x,y,z
end

function SubtractPosition(posB, posA)
	local x = posB.x - posA.x
	local y = posB.y - posA.y
	local z = posB.z - posA.z
	
	return x,y,z
end

function LightToTexture(lights, surface, world, csur)
	--local w,h = obj.w, obj.h
	rikky_module.image("w", "avi3d_lighting_a")
	local _, w, h = rikky_module.image("i", "avi3d_lighting_a")
	if (w == 0) then
		w = obj.w
	end
	if (h == 0) then
		h = obj.h
	end
	
	
	local p1 = surface[1]
	local p2 = surface[2]
	local p3 = surface[3]
	local p4 = surface[4]
	
	local Y1x, Y1y, Y1z = SubtractPosition(p4, p1)
	local Y2x, Y2y, Y2z = SubtractPosition(p3, p2)
	
	local X1x, X1y, X1z = SubtractPosition(p2, p1)
	local X2x, X2y, X2z = SubtractPosition(p4, p3)
	
	local pfunctionX = function(r, g, b, a, _, _, _, _, x, y)
		local inte = 1
		
		local nx,ny = x/w, y/h
		
		local base1 = {x = p1.x + (Y1x * ny), y = p1.y + (Y1y * ny), z = p1.z + (Y1z * ny)}
		local base2 = {x = p2.x + (Y2x * ny), y = p2.y + (Y2y * ny), z = p2.z + (Y2z * ny)}
		
		--ボツ
		--local base1X = p1.x + (Y1x * ny)
		--local base1Y = p1.y + (Y1y * ny)
		--local base1Z = p1.z + (Y1z * ny)
		
		--local base2X = p2.x + (Y2x * ny)
		--local base2Y = p2.y + (Y2y * ny)
		--local base2Z = p2.z + (Y2z * ny)
		
		local X,Y,Z = SubtractPosition(base2, base1)
		local realpos = {x = base1.x + (X * nx), y = base1.y + (Y * nx), z = base1.z + (Z * nx)}
		
		--if ((x == 1 and y == 1) or (x == 2 and y == 2)) then
		--print("X:" .. x .. ", Y:" .. y)
		--print(realpos.x .. ", " .. realpos.y .. ", " .. realpos.z)
		--print(X .. ", " .. Y .. ", " .. Z)
		--print(base1.x .. ", " .. base1.y .. ", " .. base1.z)
		--print(nx .. ", " .. ny)
		--end
		
		inte = LightPixel(realpos.x, realpos.y, realpos.z, lights, world, csur)
		
		if (world.ambient > 0) then
			inte = math.min(inte + (world.ambient / 100), 1)
		end
		
		return math.max(math.min(r * inte, 255), 0), math.max(math.min(g * inte, 255), 0), math.max(math.min(b * inte, 255), 0), a
	end
	
	rikky_module.pixelfunction(pfunctionX)
end

return {
	Draw = Draw,
	Saibunka = Saibunka,
	SetPosition = SetPosition,
	Lighting = Lighting,
	LightToTexture = LightToTexture,
	geo_shading_cache = geo_shading_cache,
	pt = pt,
}