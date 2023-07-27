

-- split
-- http://handasse.blogspot.com/2010/02/lua.html
local function split(str, d)
	-- d = d or "[,/:]"
	local p,nrep = str:gsub("%s*"..d.."%s*", "")
	return { str:match((("%s*(.-)%s*"..d.."%s*"):rep(nrep).."(.*)")) }
end

local function tonum(...)
	local v = {}
	for i=1,#({...}) do
		v[i] = tonumber(({...})[i])
	end
	return v
end

-- リーダー本体
local function readobj(filepath,reverse)
	local f = io.open(filepath,"r")

	if (f) then
		local m = split(filepath,"\\")
		local name = (m[#m]):sub(1,-5)
		local vtx = {
			filepath = filepath,
			name = name,
			layer = obj.layer,
			v  = {}, -- 頂点 {x,y,z[,w]}
			vt = {}, -- テクスチャ頂点 {u,v[,w]}
			vn = {}, -- 法線 {x,y,z}
			vp = {}, -- 空間頂点 {x,y,z}
			f  = {}  -- face頂点インデックス {v=v,vt=vt,vn=vn}
		}

		for line in f:lines() do
			local l = split(line,"%s+")
			if (l[1] == "v") then
				table.insert(vtx.v, tonum(l[2],l[3],l[4],l[5] or 1.0)) -- {x,y,z[,w]}
			elseif (l[1] == "vt") then
				table.insert(vtx.vt,tonum(l[2],l[3],l[4] or 0)) -- {u,v[,w]}
			elseif (l[1] == "vn") then
				table.insert(vtx.vn,tonum(l[2],l[3],l[4])) -- {x,y,z}
			elseif (l[1] == "vp") then
				table.insert(vtx.vp,tonum(l[2],l[3],l[4])) -- {u[,v,w]}
			elseif (l[1] == "f") then
				local face = {}

				for i=2,#l do
					local m = split(l[i],"/")
					face[i-1] = {
						v  = tonumber(m[1]),
						vt = (m[2]~="" and tonumber(m[2])) or 0,
						vn = tonumber(m[3])
					}
				end

				if (reverse) then
					local tmp = {}
					for i=1,#face do
						tmp[i] = face[#face-i+1]
					end
					face = tmp
				end

				table.insert(vtx.f,face) -- {v,[vt],vn}
			end
		end

		f:close()
		return vtx
	else
		f:close()
		return false
	end
end

--------------------------------------------------------------------------------

function loadobjectfile(file, id, reload, flipface, viewinfo)

local file = file
local model_id  = math.floor(id)
local reload    = reload
local clear_all = (reload == -2)
local reverse   = (flipface == 1)
local view_info = viewinfo

local m = split(file,"\\")
local name = (m[#m]):sub(1,-5)
local is_obj = (file:sub(-4):lower() == ".obj")

if (not loaded_obj) then
	loaded_obj = {}
end

if (reload ~= 0) then

	-- 削除
	if (reload == -1) and (loaded_obj[model_id]) then
		loaded_obj[model_id] = nil
	end

	-- 同じものが何処にもロードされていない場合に読み込む
	if (reload == 1) and (loaded_obj[model_id] == nil) then
		-- 他のidに同じものが無いかチェック
		local is_find = false
		for k,v in pairs(loaded_obj) do
			if (v.filepath == file) then
				is_find = true
				break
			end
		end

		if (not is_find) and (is_obj) then
			loaded_obj[model_id] = readobj(file,reverse)
		end
	end

	-- 強制上書き
	if (reload == 2) and (is_obj) then
		loaded_obj[model_id] = readobj(file,reverse)
	end
end


if (clear_all) then
	loaded_obj = {}
	obj.load("<s20><#dd3333>clear all<#>\n")
	obj.rx,obj.ry,obj.rz = 0,0,0
	obj.draw()
else
	if (view_info == 1) then
		local st = "ID"..string.format("%02d",model_id)..":empty"
		if loaded_obj[model_id] then
			st = "<s20><#888888>" .. loaded_obj[model_id].name .. ".obj<#>\n" ..
			"vertices : " .. #loaded_obj[model_id].v .. "\n" ..
			"faces : " .. #loaded_obj[model_id].f
		end
		obj.setfont("メイリオ",20)
		obj.load(st)
		obj.rx,obj.ry,obj.rz = 0,0,0
		obj.draw()
	elseif (view_info == 2) then
		local list = {}
		for id,v in pairs(loaded_obj) do
			table.insert(list,"ID:<#dd3422>" ..(string.format("%02d",id)).. "<#> " .. (v.name) .."<#888888> [v:"..(#v.v).."/f:"..(#v.f).. "]<#>")
		end
		table.sort(list)
		obj.setfont("メイリオ",20)
		obj.load("<s20><#888888>loaded files<#>\n"..table.concat(list,"\n"))
		obj.rx,obj.ry,obj.rz = 0,0,0
		obj.draw()
	end
end

end



--------------------------------------------------------------------------------
@描画
--------------------------------------------------------------------------------
--track0:ID,0,100,1,1
--track1:Scale,1,9000,100,0.01
--track2:Prog,-100,100,100,0.01
--track3:random,-5000,5000,0,0
--dialog:scale,local v_scl={1,1,1};点図形/fig,local figure="四角形";└ サイズ,p_s=1;└ 色/col,local p_col=0x2195f2;Texture[-2/2],local f_t=1;└ 不透明度,local f_a=1.0;└ 色/col,local f_col=0x1d86ae;線幅,local l_w=0.3;└ 不透明度,local l_a=0.2;└ 色/col,local l_c=0xff9914;noiseScale,local n_scl=100;└ wind,local n_ofs={0,0,0,0};shuffle,local sf=0;delay,local dly=10;
--check0:モデルリストの表示,0
--------------------------------------------------------------------------------

function creategeometory(id, scale, prog, rand, check0, v_scl, figure, p_s, p_col, f_t, f_a, f_col, l_w, l_a, l_c, n_scl, n_ofs, sf, dly)

-- 各種関数
local function clamp(x)
	return (x <= 0 and 0) or (x >= 1 and 1) or x
end

local function lerp(t,a,b)
	return a + (b-a)*t
end

local function linear(t,a,b,A,B)
	return clamp((t-a)/(b-a)) * (B-A)+A
end

local function sequence(t,delay,i,num)
	if (t < 0) then t,i = -t,num-i end
	return clamp( t*(1+delay*num ) - delay*i)
end

local function shuffle_tbl(tbl,seed)
	local count = #tbl
	math.randomseed(seed)
	local j
	for i = count,2,-1 do
		j = math.random(i)
		tbl[i], tbl[j] = tbl[j], tbl[i]
	end
	return tbl
end

local function set_meta(v,default)
	if not tostring(v):find("table:") then
		v = v or 0
		v = {v,v,v,v}
	end
	default = default or {v[1],v[1],v[1],v[1]}
	setmetatable(v,{__index = default})
	return v
end

local V = {}
V.sub = function(a,b) return {a[1]-b[1],a[2]-b[2],a[3]-b[3]} end
V.add = function(a,b) return {a[1]+b[1],a[2]+b[2],a[3]+b[3]} end
V.scale = function(v,x) return {v[1]*x,v[2]*x,v[3]*x} end
V.unm = function(v) return {-v[1], -v[2], -v[3]} end
V.mul = function(a,b) return {a[1]*b[1],a[2]*b[2],a[3]*b[3]} end
V.div = function(a,b) return {a[1]/b[1], a[2]/b[2], a[3]/b[3]} end
V.dot = function(a,b) return a[1]*b[1] + a[2]*b[2] + a[3]*b[3] end
V.len = function(v) return V.dot(v,v) end
V.lensq = function(a,b) return V.len( V.sub(a,b) ) end
V.length = function(v) return math.sqrt(V.dot(v,v)) end
V.normalize = function(v) return V.scale(v, 1 / V.length(v) ) end
V.cross = function(a,b) return  {a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1]} end
V.slide = function(v,n) return V.add( v,V.scale(n,-V.dot(v,n) ) ) end
V.reflect = function(v,n) return V.add( v,V.scale(n, -2 * V.dot(v,n) ) ) end
V.lerp = function(t,a,b) return { a[1]+(b[1]-a[1])*t, a[2]+(b[2]-a[2])*t, a[3]+(b[3]-a[3])*t } end
V.clamp = function(v) return {clamp(v[1]),clamp(v[2]),clamp(v[3])} end
V.ofs = function(v) return {(v[1]+1)*0.5,(v[2]+1)*0.5,(v[3]+1)*0.5} end

V.polynorm = function(a,b,c,d)
	if d then
		return V.normalize( V.cross( V.sub(c,a), V.sub(d,b) ) )
	else
		return V.normalize( V.cross( V.sub(b,a), V.sub(c,b) ) )
	end
end

V.polycentroid = function(a,b,c,d)
	if d then
		return {(a[1]+b[1]+c[1]+d[1])/4,(a[2]+b[2]+c[2]+d[2])/4,(a[3]+b[3]+c[3]+d[3])/4}
	else
		return {(a[1]+b[1]+c[1])/3,(a[2]+b[2]+c[2])/3,(a[3]+b[3]+c[3])/3}
	end
end

V.polymaxlen = function(a,b,c,d)
	if d then
		return math.sqrt( math.max( V.lensq(a,c),V.lensq(b,d)) )
	else
		return math.sqrt( math.max( V.lensq(a,b),V.lensq(b,c),V.lensq(c,a) ) )
	end
end

loaded_obj_Vector = V

local function setmtx(ang)
	local rad = math.pi/180
	ang = ang or {obj.rx,obj.ry,obj.rz}
	local cosx,sinx = math.cos(ang[1]*rad),math.sin(ang[1]*rad)
	local cosy,siny = math.cos(ang[2]*rad),math.sin(ang[2]*rad)
	local cosz,sinz = math.cos(ang[3]*rad),math.sin(ang[3]*rad)

	local function f(v)
		v[1],v[2] = (v[1]*cosz - v[2]*sinz),(v[1]*sinz + v[2]*cosz) -- z軸
		v[1],v[3] = (v[3]*siny + v[1]*cosy),(v[3]*cosy - v[1]*siny) -- y軸
		v[2],v[3] = (v[2]*cosx - v[3]*sinx),(v[2]*sinx + v[3]*cosx) -- x軸
		return v
	end
	return f
end

local function drawline(p0,p1,width,alpha,cam,t)
	t = t or 1
	local w,h = 0,0
	p1 = V.lerp(clamp(t)  ,p0,p1)
	p0 = V.lerp(clamp(t-1),p0,p1)
	local a = {p1[1]-p0[1], p1[2]-p0[2], p1[3]-p0[3]}
	local b = {cam.x-p0[1], cam.y-p0[2], cam.z-p0[3]}
	local n = {a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1]}
	local l = 1 / V.length(n) * width * 0.5
	obj.drawpoly(
		p0[1]-n[1]*l, p0[2]-n[2]*l, p0[3]-n[3]*l,
		p1[1]-n[1]*l, p1[2]-n[2]*l, p1[3]-n[3]*l,
		p1[1]+n[1]*l, p1[2]+n[2]*l, p1[3]+n[3]*l,
		p0[1]+n[1]*l, p0[2]+n[2]*l, p0[3]+n[3]*l,
		0,0,w,0,w,h,0,h,alpha
	)
end

local function drawface(a,b,c,d, ua,ub,uc,ud, w,h, s, alp)
	d = d or c
	ud = ud or uc
	-- a,b,c,d = a,d,c,b
	-- ua,ub,uc,ud = ud,uc,ub,ua
	obj.drawpoly(
	a[1]*s[1], a[2]*s[2], a[3]*s[3],
	b[1]*s[1], b[2]*s[2], b[3]*s[3],
	c[1]*s[1], c[2]*s[2], c[3]*s[3],
	d[1]*s[1], d[2]*s[2], d[3]*s[3],
	ua[1]*w,ua[2]*h, ub[1]*w,ub[2]*h,
	uc[1]*w,uc[2]*h, ud[1]*w,ud[2]*h,
	alp)
end
-------------------------------------------------------------------------------
loaded_obj = loaded_obj or {}
-- 0番目にcubemodel
loaded_obj[0] = {
	filepath = "",
	name     = "DefaultCube",
	layer    = 0,
	v = {
		{-100,-100,-100},
		{-100, 100,-100},
		{ 100,-100,-100},
		{ 100, 100,-100},
		{ 100,-100, 100},
		{ 100, 100, 100},
		{-100,-100, 100},
		{-100, 100, 100},
	},
	vn = {
		{ 0, 0,-1},{ 1, 0, 0},{ 0, 0, 1},{-1, 0, 0},{ 0, 1, 0},{ 0,-1, 0}
	},
	vt = {
		{0, 0, 0},{0, 1, 0},{1, 1, 0},{1, 0, 0}
	},
	f = {
		{{v=1,vt=1,vn=1} ,{v=2,vt=2,vn=1} ,{v=4,vt=3,vn=1} ,{v=3,vt=4,vn=1}},
		{{v=3,vt=1,vn=2} ,{v=4,vt=2,vn=2} ,{v=6,vt=3,vn=2} ,{v=5,vt=4,vn=2}},
		{{v=5,vt=1,vn=3} ,{v=6,vt=2,vn=3} ,{v=8,vt=3,vn=3} ,{v=7,vt=4,vn=3}},
		{{v=7,vt=1,vn=4} ,{v=8,vt=2,vn=4} ,{v=2,vt=3,vn=4} ,{v=1,vt=4,vn=4}},
		{{v=2,vt=1,vn=5} ,{v=8,vt=2,vn=5} ,{v=6,vt=3,vn=5} ,{v=4,vt=4,vn=5}},
		{{v=7,vt=1,vn=6} ,{v=1,vt=2,vn=6} ,{v=3,vt=3,vn=6} ,{v=5,vt=4,vn=6}}
	}
}

-------------------------------------------------------------------------------
-- モデル選択
local model_id = math.floor(id)
local vtx = loaded_obj[ model_id ]

if (check0) or (vtx == nil) then

	if (obj.index == 0) then
		local list = {}
		for k,v in pairs(loaded_obj) do
			table.insert(list, k )
		end
		table.sort(list)
		obj.setfont("メイリオ",18)
		local st = ""
		for i = 1,#list do
			if (list[i] == model_id) then
				st = st .. "<s18><#dd3422>" .. "ID:"..(string.format("%02d",list[i])).." " ..
				(loaded_obj[list[i]].name) .."<#888888> [v:".. (#loaded_obj[list[i]].v) .. "]<s><#><#>\n"
			else
				st = st .. "ID:"..(string.format("%02d",list[i])).." " ..
				(loaded_obj[list[i]].name) .. "<#888888> [v:".. (#loaded_obj[list[i]].v) .. "]<s><#><#>\n"
			end
		end
		obj.load(st)
		obj.rx,obj.ry,obj.rz = 0,0,0
		local h = obj.h / 2
		obj.draw(0,0,0,1,(vtx == nil and 0.3) or 1)
		if (vtx == nil) then
			obj.load("<s18><#2234dd>ID "..string.format("%02d",model_id).." [empty]...<#>\n")
			obj.rx,obj.ry,obj.rz = 0,0,0
			obj.draw(0,-h - obj.h/2)
		end
	else
		obj.alpha = 0
	end

end

-------------------------------------------------------------------------------
local model_scale  = scale * 0.01
local random  = rand * 0.1

-- point
local p_alp = 1
local p_size =  p_s or 1

-- face
local f_alp   = f_a or 1
local f_type  = f_t or 0
local f_time  = prog * 0.01
local f_delay = (dly or 0) * 0.01

-- line
local l_alp   = l_a or 1
local l_width = l_w or 0.5
local l_col   = l_c or 0xffffff
local l_time  = f_time
local l_delay = f_delay

-- 描画開始
if (not check0) and (vtx) then
	local rx,ry,rz = obj.rx,obj.ry,obj.rz
	local ox,oy,oz = obj.ox,obj.oy,obj.oz
	local alpha = obj.alpha
	local cam = obj.getoption("camera_param")
	local rotate = setmtx()
	obj.copybuffer("cache:org","obj")
	local nx =  1
	local ny = -1 -- y反転
	local nz =  1
	v_scl = set_meta(v_scl,{1,1,1})
	v_scl = V.scale(v_scl,model_scale)

	-- 進捗ズレ用index
	local idx = {}
	for i=1,#vtx.f do
		idx[i] = i
	end

	if (sf<0) then
		for i=1,#idx do
			idx[i] = #idx - i+1
		end
	end

	if (sf > 0) then
		shuffle_tbl(idx,sf)
	end

	-- 頂点のスケールと回転
	local v = {}
	for i=1,#vtx.v do
		v[i] = rotate({
			vtx.v[i][1]*v_scl[1] * nx,
			vtx.v[i][2]*v_scl[2] * ny,
			vtx.v[i][3]*v_scl[3] * nz,
			vtx.v[i][4]
		})
	end

	---- random
	if (random > 0) then
		local r_amount = random * 10
		local r_vec = {1, 1, 1}
		local r_bias = 0.5 * 1
		r_vec = V.scale(r_vec,r_amount)
		math.randomseed(12434 + obj.index)
		local rx,ry,rz
		for i=1,#v do
			rx = math.random() - r_bias
			ry = math.random() - r_bias
			rz = math.random() - r_bias
			v[i] = {
				v[i][1] + r_vec[1] * rx ,
				v[i][2] + r_vec[2] * ry ,
				v[i][3] + r_vec[3] * rz ,
				v[i][4]
			}
			v[i].random = {rx,ry,rz}
		end
	end

	--- noise
	if (random < 0) then
		require("simplexnoise_utl")
		local fbm = simplexnoise_utl.fbm
		local octaves, lacunarity, gain = 1,3,0.35
		n_scl = set_meta(n_scl,{10,10,10,10})
		n_scl = {n_scl[1]*0.0005,n_scl[2]*0.0005,n_scl[3]*0.0005,n_scl[4]*0.0005}
		n_ofs = set_meta(n_ofs,{0,0,0,0})
		local nt = -obj.time
		local n_dimension = (n_ofs[4]~=0 and 4) or 3
		local n_evolution = n_scl[4] * (n_ofs[4] or 0) * nt + 1
		n_ofs = {n_ofs[1]*nt+1 ,n_ofs[2]*nt+2, n_ofs[3]*nt+3}

		local n_amo = random * 20
		local n_offset = V.add(n_ofs,{obj.x+ox,obj.y+oy,obj.z+oz})

		local nx,ny,nz
		for i=1,#v do
			local vn = V.mul( V.add(v[i],n_offset), n_scl)
			nx = fbm(n_dimension, {vn[1],vn[2],vn[3]+3, n_evolution+1}, octaves, lacunarity, gain)
			ny = fbm(n_dimension, {vn[1]+3,vn[2],vn[3], n_evolution+2}, octaves, lacunarity, gain)
			nz = fbm(n_dimension, {vn[1],vn[2]+3,vn[3], n_evolution+3}, octaves, lacunarity, gain)
			v[i][1] = v[i][1] + n_amo * nx
			v[i][2] = v[i][2] + n_amo * ny
			v[i][3] = v[i][3] + n_amo * nz
			v[i].random = {nx,ny,nz}
		end
	end

	-- ---- pushapart コメントを外せば動作する
	-- local radius = 300
	-- local strength = 1
	-- local attract = 0
	-- local v0 = {0,0,0} -- 押し出し基準位置座標
	-- v0 = V.sub(v0,{obj.x,obj.y,obj.z})
	-- for i=1,#v do
	-- 	local vc = {v[i][1]-v0[1],v[i][2]-v0[2],v[i][3]-v0[3]}
	-- 	local len = V.length(vc)
	-- 	len =  (len <= 1e-6) and 1e-6 or len
	-- 	if (attract~=1 and len<radius) or (attract==1 and len>radius) then
	-- 		local push = 1/len * math.max((radius - len) * strength,-len)
	-- 		v[i] = {v[i][1]+vc[1]*push, v[i][2]+vc[2]*push, v[i][3]+vc[3]*push, v[i][4]}
	-- 		v[i].push_pow = len / radius
	-- 	else
	-- 		v[i].push_pow = 0
	-- 	end
	-- end

	-- カメラ深度もどき
	if (loaded_obj_Depth) then
		local depth,sign = 0,1
		local org_rgb = { RGB(p_col) }
		for i=1,#v do
			depth,sign = loaded_obj_Depth.camera_depth(v[i])
			v[i].d_col = loaded_obj_Depth.depth_color(depth, sign, org_rgb)
			v[i].alpha = (loaded_obj_Depth.alp_fade==1 and (alpha * (1-depth)) ) or alpha
			v[i].d_depth = depth
			v[i].d_sign  = sign
		end
	end

	-- 面描画
	if (f_alp ~= 0) then
		if (f_alp < 0) then
			obj.setoption("antialias",0)
			f_alp = math.abs(f_alp)
		end

		obj.setoption("billboard",0)
		local f_maxdst, f_fade = 1000,400
		if (f_type >= 1) then

			if (f_type == 2) then
				obj.load("image",vtx.filepath:gsub(".obj",".png"))
			else
				obj.copybuffer("obj","cache:org")
			end
			obj.effect("反転","左右反転",0,"上下反転",1)
			local w,h = obj.h,obj.w
			local aspect = w/h
			obj.rx,obj.ry,obj.rz = 0,0,0
			obj.ox,obj.oy,obj.oz = ox,oy,oz
			obj.alpha = alpha

			for n,f in ipairs(vtx.f) do
				local alp = f_alp
				local seq = sequence(f_time,f_delay,idx[n]-1,#vtx.f-1)
				if (loaded_obj_Depth) then
					alp = v[ f[1].v ].alpha
				end
				-- seq = seq*seq*(3-2*seq)
				-- obj.ry = (1-seq)*90
				-- obj.zoom = seq
				-- obj.oy = (1-seq)*120
				drawface(
				v[ f[1].v ],
				v[ f[2].v ],
				v[ f[3].v ],
				v[ (f[4] or f[3]).v ],
				vtx.vt[ f[1].vt ],
				vtx.vt[ f[2].vt ],
				vtx.vt[ f[3].vt ],
				vtx.vt[ (f[4] or f[3]).vt ],
				w,h*aspect,
				{1,1,1},seq * alp
			)
			end

		else

			obj.load("figure","四角形",f_col,1)
			local u1,v1 = 0,0
			-- if (loaded_obj_Depth) then
			-- 	obj.load("figure","四角形",f_col,2)
			-- 	u1,v1 = 2,2
			-- end
			obj.rx,obj.ry,obj.rz = 0,0,0
			obj.ox,obj.oy,obj.oz = ox,oy,oz
			obj.alpha = alpha
			local norm = {0,0,0}
			local f_norm = rotate({0,0,1})
			local f_rgb = {RGB(f_col)}
			for n,f in ipairs(vtx.f) do
				local rgb = f_rgb
				local alp = alpha
				local seq = sequence(f_time,f_delay,idx[n]-1,#vtx.f-1)
				local maxlen = V.polymaxlen(v[f[1].v], v[f[2].v], v[f[3].v], (f[4] and v[f[4].v]))
				alp = linear(maxlen, f_maxdst,(f_maxdst + f_fade), alp, 0)^2
				if (seq * alp > 0) then
					-- seq = seq*seq*(3-2*seq)
					-- obj.ry = (1-seq)*90
					-- obj.zoom = n_x --seq
					-- obj.oy = (1-seq)*120
					if (f_type == -1) then
						norm = V.scale( V.ofs ( V.mul( V.polynorm(v[f[1].v],v[f[2].v],v[f[3].v],(f[4] and v[f[4].v]) or nil ),{-1,1,-1}) ) ,255) -- aviutl上での法線
						rgb = norm
					elseif (f_type == -2) then
						norm = V.scale( V.ofs( (vtx.vn[ f[1].vn ]) or f_norm ) ,255) -- モデル法線なんか呼び出しがおかしい
						rgb = norm
					end
					col = RGB(rgb[1],rgb[2],rgb[3])
					obj.putpixel(0,0,col,alp)
					if (loaded_obj_Depth) then
						local depth = v[f[1].v].d_depth
						col = loaded_obj_Depth.depth_color(depth, v[ f[1].v ].d_sign, rgb)
						alp = v[ f[1].v ].alpha
						local a = col
						obj.putpixel(0,0,col,alp)
					end

					drawface(
					v[ f[1].v ],
					v[ f[2].v ],
					v[ f[3].v ],
					v[ (f[4] or f[3]).v ],
					{0,  0},
					{u1, 0},
					{u1,v1},
					{0, v1},
					1,1,
					{1,1,1},seq * f_alp * alp
				)
				end
			end

		end

	end

	-- 点描画
	if (p_alp * p_s>0) then
		obj.setoption("antialias",1)
		obj.setoption("billboard",3)
		local p_line = 1000
		if (loaded_obj_Depth) then
			-- obj.copybuffer("cache:fig","obj")
			for i=1,#v do
				local tn = 1 -- sequence(l_time,l_delay,i-1,#v-1)
				-- obj.copybuffer("obj","cache:fig")
				obj.load("figure",figure, v[i].d_col, p_s, p_line)
				obj.ox,obj.oy,obj.oz = ox,oy,oz
				obj.alpha = alpha
				if (p_alp * tn * v[i].alpha > 0) then
					loaded_obj_Depth.depth_blur(v[i].d_depth)
				end
				obj.draw(v[i][1],v[i][2],v[i][3],1,p_alp*tn * v[i].alpha,-rx,-ry,-rz)
			end
		else
			if (figure == "背景") then
				figure = "四角形"
			end
			obj.load("figure",figure, p_col, p_s, p_line)
			obj.ox,obj.oy,obj.oz = ox,oy,oz
			obj.alpha = alpha
			for i=1,#v do
				local tn = 1 -- sequence(l_time,l_delay,i-1,#v-1)
				obj.draw(v[i][1],v[i][2],v[i][3],1,p_alp*tn,-rx,-ry,-rz)
			end
		end

	end

	-- 線描画
	if (l_alp * l_width ~= 0) then
		obj.setoption("billboard",0)
		obj.load("figure","四角形",l_col,1)
		if (l_alp < 0) then
			obj.setoption("antialias",0)
			l_alp = math.abs(l_alp)
		end
		obj.rx,obj.ry,obj.rz = 0,0,0
		obj.ox,obj.oy,obj.oz = ox,oy,oz
		obj.alpha = alpha
		if (l_time~=0) then
			for n,f in ipairs(vtx.f) do
				local seq  = sequence(l_time,l_delay,idx[n]-1,#vtx.f-1)
				if (seq * l_alp > 0) then
					for i=1,#f do
						-- obj.putpixel(0,0,RGB(255 * (push_pow[ f[i].v ] or 0),200,200),1)
						drawline(v[ f[i].v ],v[ f[i%(#f)+1].v ],l_width,v[ f[1].v ].alpha or l_alp,cam,seq)
					end
				end
			end
		end

	end

	obj.rx,obj.ry,obj.rz = rx,ry,rz
	obj.ox,obj.oy,obj.oz = ox,oy,oz
	obj.alpha = alpha

end

-- オプションデータ削除
loaded_obj_Depth = nil

-- ダイアログの値も一応削除(local宣言するスペースがなくてグロい)
p_s, f_a, f_t, l_a, l_w, l_c = nil,nil,nil,nil,nil,nil

end