local Sub	= function(a,b) return {a[1]-b[1],a[2]-b[2],a[3]-b[3]} end
local Add	= function(a,b) return {a[1]+b[1],a[2]+b[2],a[3]+b[3]} end
local Scale	= function(v,s) return {v[1]*s,v[2]*s,v[3]*s} end
local Mul	= function(a,b) return {a[1]*b[1],a[2]*b[2],a[3]*b[3]} end
local Unm   = function(v) return {-v[1], -v[2], -v[3]} end
local Div	= function(a,b) return {a[1]/b[1], a[2]/b[2], a[3]/b[3]} end
local Len	= function(v) return (v[1]*v[1]+v[2]*v[2]+v[3]*v[3]) end
local Lensq	= function(a,b) return Len( Sub(a,b) ) end
local Length	= function(v) return (v[1]*v[1]+v[2]*v[2]+v[3]*v[3])^.5 end
local Normalize	= function(v) local l=Length(v); return {v[1]/l,v[2]/l,v[3]/l},l end
local Cross	= function(a,b) return  {a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1]} end
local Dot	= function(a,b) return (a[1]*b[1] + a[2]*b[2] + a[3]*b[3]) end
local Slide   = function(v,n) return Add(v,Scale(n,-Dot(v,n))) end
local eps	= 1e-08
local Reflect = function(v,n) -- R = -2(n・v)*n + v or R = v - 2(n・v)n
	-- return Add(v,Scale(n,-2*Dot(v,n))) end
	 local d = 2*(v[1]*n[1] + v[2]*n[2] + v[3]*n[3])
	 return {v[1]-n[1]*d,  v[2]-n[2]*d, v[3]-n[3]*d}
end
local Snell = function(ray,IOR)
	local cos2t = 1 - nnt*nnt * (1-ddn*ddn)
end
local Set = {}
Set.param = function()
	return {
		p  = {obj.x, obj.y, obj.z},
		c  = {obj.cx,obj.cy,obj.cz},
		r  = {obj.rx,obj.ry,obj.rz},
		po = {obj.ox,obj.oy,obj.oz},
		pa = {obj.x+obj.ox, obj.y+obj.oy, obj.z+obj.oz}
	}
end
Set.pos   = function() return {obj.x, obj.y, obj.z} end
Set.pos_o = function() return {obj.ox, obj.oy, obj.oz} end
Set.pos_a = function() return {obj.x+obj.ox,obj.y+obj.oy,obj.z+obj.oz} end
Set.rot   = function() return {obj.rx,obj.ry,obj.rz} end
Set.layer = function(l) return {obj.getvalue("layer"..l..".x"),obj.getvalue("layer"..l..".y"),obj.getvalue("layer"..l..".z")} end

local type = function(v)
	local s = tostring(v)
	if(s == v)then return "string" end
	if(s == "nil")then return "nil" end
	if(s == "true" or s == "false")then return "boolean" end
	if(string.find(s, "table:"))then return "table" end
	if(string.find(s, "function:"))then return "function" end
	if(string.find(s, "userdata:"))then return "userdata" end
	return "number"
end

-- カメラ平面作成---------------------------------------------------
-- 引数なしでカメラ目標の距離に平面、offsetがあるとカメラから1024+offsetの位置
local Norm_cameraplane = function(offset)
    local c = obj.getoption("camera_param")
    local n,d = Normalize( Sub( {c.tx,c.ty,c.tz}, {c.x,c.y,c.z} ))
    if offset then d=1024+offset end
    return {n[1],n[2],n[3],d = d}
end

-- 3点a,b,cのポリゴン法線,重心 ----------------------------
local Norm_surface = function(a,b,c)
    local AB=Sub(b,a)
    local BC=Sub(c,b)
    local center = {(a[1]+b[1]+c[1])/3,(a[2]+b[2]+c[2])/3,(a[3]+b[3]+c[3])/3}
    local N = Normalize(Cross(AB,BC))
    return N,center
end

-- 点Aと平面上の最近点(A=座標 P=平面上の点 n=平面の法線 )----------------------------
-- 平面上の点 p = Scale(N,d)
local Pos_p_on_plane = function(a,p,n)
    local PA = {a[1]-p[1],a[2]-p[2], a[3]-p[3]}
    local d = (n[1]*PA[1] + n[2]*PA[2] + n[3]*PA[3])
    return {a[1]-(n[1]*d), a[2]-(n[2]*d), a[3]-(n[3]*d)}
end

-- 点pと面abc上の最近点------------------------------------
local Pos_p_on_poly = function(p,a,b,c)
    local AB = Sub(b,a)
    local BC = Sub(c,b)
    local n = Normalize(Cross(AB,BC))
    --local d = Dot( n, Sub(p,a) )
    --return Sub(a,Scale(n,d)),n
    return Pos_p_on_plane(p,a,n)
end

-- 点Pと線abの距離------------------------------------
local Dist_p_line = function(P,a,b)
    local ab,aP = Sub(b,a), Sub(P,a)
    local cross = Cross(ab,aP)
    local len = Length(ab)
    if len==0 then len=eps end -- 0除算でnanが出るので
    local dst   = Length(cross) / len
    return dst
end

-- 点Pと線分abの距離------------------------------------
local Dist_p_segment = function(P,a,b)
    local AB = Sub(b,a)
    if ( Dot(AB,Sub(P,a) ) < eps ) then
        return Length(Sub(a,P))
    elseif ( Dot(AB,Sub(P,b) ) > eps ) then
        return Length(Sub(b,P))
    else
        return Dist_p_line(P,a,b)
    end
end


-- 点Pと線ab上の最近点------------------------------------
local Pos_p_on_line=function(P,a,b)
    local AB = Sub(b,a)
    local N = Normalize(AB) 	--線abの単位ベクトル
    local D = Dot(N, Sub(P,a))	--aPベクトルと内積
    return Add(a,Scale(N,D))
end

-- 点Pと線分ab上の最近点------------------------------------
local Pos_p_on_segment=function(P,a,b)
    local AB = Sub(b,a)
    if	( Dot(AB,Sub(P,a) ) < eps ) then
        return a
    elseif	( Dot(AB,Sub(P,b) ) > eps ) then
        return b
    else
        return Pos_p_on_line(P,a,b)
    end
end


-- 線分ABと平面の交点------------------------------------
--Intersect_plane_Line
local Pos_plane_intersection_segment = function(
    A,-- 線分始点
    B,-- 線分終点
    n,-- 平面法線
    d,-- 平面法線の長さ
    PL-- = {n[1],n[2],n[3],d=d} -- ax+by+cz-d=0
    )
    --平面上の点P
    local P = Scale(n,d)
    local PA = Sub(A,P)
    local PB = Sub(B,P)
    --平面法線と内積
    local dot_PA = Dot(PA,n)
    local dot_PB = Dot(PB,n)
    --線端が平面上にあった時の誤差を0に
    if math.abs(dot_PA) < eps then  dot_PA = 0 end
    if math.abs(dot_PB) < eps then  dot_PB = 0 end
    --交差判定
    if (dot_PA == 0) and (dot_PB == 0) then
    -- 線端が平面上で計算不可
        return false
    elseif  ((dot_PA >= 0) and (dot_PB <= 0)) or ((dot_PA <= 0) and (dot_PB >= 0))  then
    -- 内積正負が異なれば交差
        local AB = Sub(B,A)
    -- 交点とAの距離 交点とBの距離 = dot_PA , dot_PB
        local ratio = math.abs(dot_PA) / ( math.abs(dot_PA) + math.abs(dot_PB) )
        return {
            A[1] + ( AB[1] * ratio ),
            A[2] + ( AB[2] * ratio ),
            A[3] + ( AB[3] * ratio )
        }
    else
    --交点なし
        return false
    end

end

--線AB,線CDの交点(なければ最近点)------------------------------------
local Pos_intersection_2line = function(A,B,C,D)
    local AB = Sub(A,B)
    local CD = Sub(C,D)
    --計算不可
    if( Len(AB)==0) or (Len(CD)==0) then return 0,nil,nil end
    local n1 = Normalize(AB)
    local n2 = Normalize(CD)
    local w1 = Dot( n1, n2 )
    local w2 = 1 - w1*w1
    if( w2 == 0 ) then  return 0,false,false end
    local AC = Sub(A,C)
    local d1 = (Dot(AC,n1)-w1*Dot(AC,n2)) / w2
    local d2 = (w1*Dot(AC,n1)-Dot(AC,n2)) / w2
    local ret1,ret2
    --AB上の最近点
    ret1 = Add(A,Scale(n1,d1))
    --BC上の最近点
    ret2 = Add(C,Scale(n2,d2))
    if( Len(Sub(ret1,ret2)) < eps ) then
        return 1,ret1,ret2 --交点
    else
        return 2,ret1,ret2 --交点なし、最近点
    end
end

--線(p1,p2)と線(p3,p4)の交点(なければnil)------------------------------------
-- crosspoiont = {x1,y1} + ramda * {x2-x1,y2-y1}
local function Pos_intersection_2line_n(p1,p2,p3,p4)
	local k = (p4[2]-p3[2])*(p4[1]-p1[1]) - (p4[1]-p3[1])*(p4[2]-p1[2])
	local e = (p2[1]-p1[1])*(p4[2]-p1[2]) - (p2[2]-p1[2])*(p4[1]-p1[1])
	local d = (p2[1]-p1[1])*(p4[2]-p3[2]) - (p2[2]-p1[2])*(p4[1]-p3[1])
	local r = k / d
	local m = e / d
	if ( ( r >= eps and r <= 1 ) and ( m >= eps and m <= 1 ) ) then
        return {
            p1[1] + r*(p2[1]-p1[1]),
            p1[2] + r*(p2[2]-p1[2]),
            p1[3] + r*(p2[3]-p1[3])
        }
    else
    	return nil
    end
end

--法線との反射ベクトル s=法線(正規化),v=入射ベクトル------------------------------------
local Vec_reflect=function(s,v)
    local t = -2*(s[1]*v[1] + s[2]*v[2] + s[3]*v[3])/(s[1]*s[1] + s[2]*s[2] + s[3]*s[3])
    return {v[1]+(t*s[1]), v[2]+(t*s[2]), v[3]+(t*s[3])}
end


--回転行列----------------------------------------------------------------------
local sin,cos,RAD,DEG = math.sin,math.cos,math.pi/180,180/math.pi

local Rot = function(v,r)
	v = v or {0,0,-1}
	r = r or {obj.rx,obj.ry,obj.rz}
    local rx,ry,rz = r[1]*RAD, r[2]*RAD, r[3]*RAD
    local cosx,sinx = cos(rx),sin(rx)
    local cosy,siny = cos(ry),sin(ry)
    local cosz,sinz = cos(rz),sin(rz)
	local x,y,z = v[1],v[2],v[3]
    x,y = (x*cosz - y*sinz),(x*sinz + y*cosz) -- z_axis
    z,x = (z*cosy - x*siny),(z*siny + x*cosy) -- y_axis
    y,z = (y*cosx - z*sinx),(y*sinx + z*cosx) -- X_axis
	return x,y,z
end

--相対座標、中心点を移動後に回転行列----------------------------------------------------------------------
local Rotc = function(pos,anc,rot)
	pos = pos or {obj.ox, obj.oy, obj.oz}
	anc = anc or {obj.cx, obj.cy, obj.cz}
	rot = rot or {obj.rx, obj.ry, obj.rz}
	local zoom = obj.getvalue("zoom")*.01
	local ox,oy,oz = pos[1],pos[2],pos[3]
	local cx,cy,cz = anc[1]*zoom,anc[2]*zoom,anc[3]*zoom
	local x,y,z = Rot({ox-cx, oy-cy, oz-cz},rot)
	return  x+cx, y+cy, z+cz
end

--vをロール(r)、ピッチ(p)、ヨー(Y)で回転(回転角はラジアン)
local Rot_rpy = function(v,r,p,Y)
    local Sin = {r=sin(r),p=sin(p),y=sin(Y)}
    local Cos = {r=cos(r),p=cos(p),y=cos(Y)}
    local x,y,z = v[1],v[2],v[3]
    local x0 = x*Cos.p*Cos.r + y*( Sin.y*Sin.p*Cos.r - Cos.y*Sin.r ) + z*( Sin.y*Sin.r + Cos.y*Sin.p*Cos.r )
    local y0 = x*Cos.p*Sin.r + y*( Sin.y*Sin.p*Sin.r + Cos.y*Cos.r ) + z*(-Sin.y*Cos.r + Cos.y*Sin.p*Sin.r )
    local z0 = -x*(Sin.p) + y*(Sin.y*Cos.p) + z*(Cos.y*Cos.p)
    return x0,y0,z0
end

--任意単位ベクトル周り回転(回転角はラジアン)
local Rot_v = function(v,r)
    local x,y,z = v[1],v[2],v[3]
    scale = scale or 1
    local cosr = cos(r)
    local sinr = sin(r)
    local x0 = x*x*(1-cosr)+cosr   + x*y*(1-cosr)-z*sinr + z*x*(1-cosr)+y*sinr
    local y0 = x*y*(1-cosr)+z*sinr + y*y*(1-cosr)+cosr   + y*z*(1-cosr)-x*sinr
    local z0 = z*x*(1-cosr)-y*sinr + y*z*(1-cosr)+x*sinr + z*z*(1-cosr)+cosr
    return {x0,y0,z0}
end

--球形配置coordinates r = radius, p = math.pi*2/hn*i + mathpi/2 , t = math.pi/hn*j
local Co_spherical = function(r,p,t)
    local x,y,z = sin(t)*cos(p), sin(t)*sin(p), cos(t)
    --local x,y,z = sin(t)*sin(p)*radius, -cos(t)*radius, sin(t)*cos(p)*radius
    return y*r,-z*r,x*r
end

--トーラス配置,R=Radius, r=raidus, p = math.pi*2/hn*i + math.pi/2 , t = math.pi/hn*j
local Co_torus = function(R,r,p,t)
    local x = R*cos(t) + r*cos(p)*cos(t)
    local y = R*sin(t) + r*cos(p)*sin(t)
    local z = r*sin(p)
    return x,y,z
end

--パラボライダル u=(0~ ) ,v=(0~ ),r=(0 <= math.pi*2)
local Co_Paraboloidal=function(u,v,r)
    local x = u*v*cos(r)
    local y = u*v*sin(r)
    local z = (u*u-v*v)/2
    return x,y,z
end

-- Prolate spheroidal coordinates xi=[0,huge) ,n=[0,math.pi), p=[0,math.pi*2)
local Co_prolate_spheroidal=function(xi,n,p,a)
    a = a or 1
    local x = a * math.sinh(xi)*sin(n)*cos(p)
    local y = a * math.sinh(xi)*sin(n)*sin(p)
    local z = a * math.cosh(xi)*cos(n)
    return x,y,z
end

local draw_spherical = function(wn,hn,r)
	local t = math.pi*2/wn
	local p = math.pi/hn
	local v = {}
	for i=0,wn-1 do
		v[i+1]={}
		local sint = math.sin(t*i)
		local cost = math.cos(t*i)
		for j=0,hn-1 do
			local sinp = math.sin(p*j)
			local cosp = math.cos(p*j)
			local x,y,z = sint*cosp*r, sint*sinp*r, cost*r
			v[i+1][j+1] = {-y,z,x}
			obj.draw(-y,z,x)
		end
	end
	return v
end

local draw_torus = function(wn,hn,R,r)
	local t = math.pi*2/wn
	local p = math.pi*2/hn
	local v = {}
	for i=0,wn-1 do
		v[i+1]={}
		local sint = math.sin(t*i)
		local cost = math.cos(t*i)
		for j=0,hn-1 do
			local sinp = math.sin(p*j)
			local cosp = math.cos(p*j)
			local x,y,z = cost*R + (cosp*cost*r), sint*R + (cosp*sint*r), sinp*r
			v[i+1][j+1] = {x,y,z}
			obj.draw(x,y,z)
		end
	end
	return v
end

--他のレイヤー座標をまとめる---------------------------------------------------

local GL = function(...)
    local tx = {[0]=".x",".y",".z"}
    local V,A,n = {},{},3
    for k=1,select("#",...) do
        A[k]={}
        for i=0,n-1 do
            local val = obj.getvalue("layer"..select(k,...)..tx[i])
            V[k*n+i-n+1] = val
            A[k][i+1] = val
        end
    end
    return V,A
end

local GL2 = function(...)
    local tx = {[0]=".x",".y",".z"}
    local V,A,n = {},{},3
    for k=1,select("#",...) do
        A[k]={}
        for i=0,n-1 do
            local val = obj.getvalue("layer"..select(k,...)..tx[i])
            --V[k*n+i-n+1] = val
            A[k][i+1] = val
        end
    end
    return A
end

local Param = function(param)
    if tostring(param):find("table:") then
        obj.ox,obj.oy,obj.oz,obj.zoom,obj.alpha,obj.cx,obj.cy,obj.cz,obj.aspect=unpack(param)
    elseif param==0 then
        obj.ox,obj.oy,obj.oz,obj.zoom,obj.alpha,obj.aspect,obj.cx,obj.cy,obj.cz = 0,0,0,1,1,0,0,0,0
    else
        return {obj.ox,obj.oy,obj.oz,obj.zoom,obj.alpha,obj.cx,obj.cy,obj.cz,obj.aspect}
    end
end

local Camparam = function()
	local c = obj.getoption("camera_param")
	c.pos = {c.x,c.y,c.z}
	c.tgt = {c.tx,c.ty,c.tz}
	c.eye = {c.tx-c.x,c.ty-c.y,c.tz-c.z}
	c.l = math.sqrt(c.eye[1]*c.eye[1] + c.eye[2]*c.eye[2] + c.eye[3]*c.eye[3])
	if c.l==0 then
		c.n = {0,0,-1}
	else
		c.n = {c.eye[1]/c.l, c.eye[2]/c.l, c.eye[3]/c.l}
	end
	return c
end


local function Clamp(x)
	return (x<=0 and 0) or (x>=1 and 1) or x
end

local function Linear(t,a,b,A,B)
	return Clamp((t-a)/(b-a)) * (B-A) + A
end

local function LinearN(t,a,b,A,B)
	return (t-a)/(b-a) * (B-A) + A
end

local function Smoothstep(v)
  return v*v*(3-2*v)	--MAD, MUL, MUL
end

local function Sequence(t,delay,i,num) 	-- i = [1 ~ num]
    local total= 1 + (num-1)*delay
    i = i-1
    if (t < 0) then t,i = -t,num-1-i end
    if (delay == 0) then return t end
    local life = 1+(delay*(num-1))
    return Clamp(t*life-delay*i)
end

local TBL = function(v,default)
    --vがnilなら0を(defaultを指定すればn)、
    --テーブルならobj.indexで振り分け(要素がobj.numより少なければループ)
    --それ以外ならそのまま返します。
    if default==nil then default=0 end
    local V=tostring(v)
    if v==nil then
        return default
    elseif not string.find(V,"table:")then
        return v
    else
        return v[obj.index % (#v) + 1]
    end
end

local Splt = function(v,default)
	local tb = {H=1,S=2,L=3,A=4,U=5,V=6,W=7,X=5,Y=6,Z=7,R=8}
	local ret = TBL(v,default)
	if (tostring(ret)==ret) and not ret:find("%d") then
		if #ret==0 then
			return 0
		else
			ret = ret:gsub("[%s]",""):upper()
			if ret:find("-") then
				return -tb[ret:gsub("-","")]
			else
				return tb[ret]
			end
		end
	end
	return tonumber(ret)
end


local Swap_table = function(t,a,b)
    if t==0 then return a end
    if t==1 then return b end
    local t1 = 1-t
    return {
        a[1]*t1+b[1]*t,
        a[2]*t1+b[2]*t,
        a[3]*t1+b[3]*t
    }
end

local Swap_col=function(t,cola,colb)
    local a={RGB(cola)}
    local b={RGB(colb)}
    local c = Swap_table(t,a,b)
    return RGB(c[1],c[2],c[3]),c
end

local Swap_i_table = function(t,...)
	t=t-1
	local i,d = math.modf(t)
	local len = select("#",...)
    if d==0 then return select(i%len+1,...) end
	local a = select(i%len+1,...)
	local b = select((i+1)%len+1,...)
    return {
        a[1]*(1-d) + b[1]*d,
        a[2]*(1-d) + b[2]*d,
        a[3]*(1-d) + b[3]*d
    }
end

  --点p0と点p1を結ぶ直線(2D)----------------------------------------------------------------------
  local Draw_line = function(p0,p1,width,col,alp,st,va,t)
  	width = width or 1
  	width = width*.5
  	st = st or 1500
  	va = va or 2500
  	local x0,y0=p0[1],p0[2]
  	local x1,y1=p1[1],p1[2]
  	local x,y=(x1-x0),(y1-y0)
  	local L=(x*x+y*y)^.5

  	if L>(st+va) then
  		return 0
  	else
  		t = t or 1
  		t = math.max(0,math.min(1,t))
  		local mul=function(v,s) return {v[1]*s,v[2]*s} end
  		local add=function(a,b) return {a[1]+b[1],a[2]+b[2]} end
  		p1 = add( mul(p1,t), mul(p0,1-t))
  		x1,y1 = p1[1],p1[2]
    		local l = L-st
   		l = l<0 and 0 or l
   		l = l>va and va or l
  		l = (1-l/va)^2
   		width = width*l
   		local xc,yc= -(y/L)*width, (x/L)*width
    		if col then obj.putpixel(0,0,col,1) end
    		obj.drawpoly(
    			x0+xc,y0+yc,0,
     			x1+xc,y1+yc,0,
     			x1-xc,y1-yc,0,
     			x0-xc,y0-yc,0,
     			0,0, 0,0, 0,0, 0,0,alp or 1
   	 	)
  		return 1
   	end
  end

  --p0,p1を結ぶ直線(3D,カメラ用)----------------------------------------------------------------------

  local Draw_line3D = function(
  	p0,	    --座標 {x,y,z}
  	p1,	    --座標 {x,y,z}
  	width,	--[線幅]
  	col,	--[色]
  	alp,	--[透明度]
  	st,	    --[消滅開始距離]
  	va,	    --[消滅までのフェード範囲]
  	nst,	--[消滅開始距離(st以下)]
  	nva,	--[消滅までの範囲(nst以下～0まで)]
  	t,	    --[0~1で線を伸ばす]
    cam_param
  	)
  	width = width or 1
  	alp = alp or 1
  	st,va = st or 500, va or 1000
  	nst,nva = nst or 1 ,nva or 0
  	t = t or 1
  	t = math.max(0,math.min(1,t))
    t = t*t
  	p0[3] = p0[3] or 0
  	p1[3] = p1[3] or 0
  	local a=Sub(p1,p0)
  	local len=Length(a)
  	if len>(st+va) then return 0,p1 end
  	if len<(nst-nva) then return 0,p1 end
  	local lin = Linear(len,st,va,1,0)
  	--lin = lin*Linear(len,nva,nst ,0,1)
  	width = width*lin
  	alp = alp*lin
  	if col then obj.putpixel(0,0,col,1) end
  	local c = cam_param or obj.getoption("camera_param")
  	local b={c.x-p0[1], c.y-p0[2], c.z-p0[3]}
  	local n = Cross(a,b)
  	local l = Length(n)
  	local nx,ny,nz = (n[1]/l)*width*.5, (n[2]/l)*width*.5 ,(n[3]/l)*width*.5
  	p1 = Add( Scale(p1,t), Scale(p0,1-t))
  	obj.drawpoly(
  		p0[1]-nx,p0[2]-ny,p0[3]-nz,
  		p1[1]-nx,p1[2]-ny,p1[3]-nz,
  		p1[1]+nx,p1[2]+ny,p1[3]+nz,
  		p0[1]+nx,p0[2]+ny,p0[3]+nz,
  		0,0,0,0,0,0,0,0,math.min(1,alp)
  		)
  	return 1
  end

  local Draw_line3D2 = function(
  	p0,	--座標 {x,y,z}
  	p1,	--座標 {x,y,z}
  	width,	--[線幅]
  	col,	--[色]
    col2,
  	alp,	--[透明度]
    alp2,
  	st,	--[消滅開始距離]
  	va,	--[消滅までのフェード範囲]
  	nst,	--[消滅開始距離(st以下)]
  	nva,	--[消滅までの範囲(nst以下～0まで)]
  	t,	--[0~1で線を伸ばす]
    cam_param
  	)
  	width = width or 1
  	st,va = st or 500,va or 1000
  	nst,nva = nst or 1 ,nva or 0
  	t = t or 1
  	t = math.max(0,math.min(1,t))
  	p0[3] = p0[3] or 0
  	p1[3] = p1[3] or 0
  	local a=Sub(p1,p0)
  	local len=Length(a)
  	if len>(st+va) then return 0,p1 end
  	if len<(nst-nva) then return 0,p1 end
    alp2 = alp2 or alp1
    col2 = col2 or col
  	local lin = Linear(len,st,va,1,0)
  	lin = lin*Linear(len,nva,nst ,0,1)
  	width = width*lin
  	local alpha = math.max(alp,alp2)--lin
  	obj.putpixel(0,0,col,alp)
  	obj.putpixel(1,0,col2,alp2)
    obj.putpixel(2,0,col2,alp2)
  	local c = cam_param or obj.getoption("camera_param")
  	local b={c.x-p0[1], c.y-p0[2], c.z-p0[3]}
  	local n = Cross(a,b)
  	local l = Length(n)
  	local nx,ny,nz = (n[1]/l)*width*.5, (n[2]/l)*width*.5 ,(n[3]/l)*width*.5
  	p1 = Add( Scale(p1,t), Scale(p0,1-t))
  	obj.drawpoly(
  		p0[1]-nx,p0[2]-ny,p0[3]-nz,
  		p1[1]-nx,p1[2]-ny,p1[3]-nz,
  		p1[1]+nx,p1[2]+ny,p1[3]+nz,
  		p0[1]+nx,p0[2]+ny,p0[3]+nz,
  		0,0, 2,0, 2,0, 0,0,alpha
  		)
  	return 1
  end

  --座標の入ったテーブルtを受け取って線で結ぶ----------------------------------------------------------------------
  -- mode = 0 :順 	maxnum :増えると順に引く、nilで一周。
  -- mode = 1 :すべて巡回 maxnum :増えると順に引く、ネズミ算式に増える。テーブルで第二引数で点あたりの最大数を制限 ・ 省略時は1(mode 0と同じ)
  -- mode = 2 :すべて巡回 maxnum :点一つ当たり何本引くか指定のみ。
  -- tは {{x,y,z},{x,y,z},{x,y,z}...}の形式。 一応 {x,y,z,x,y,z,x,y,z...}でも可。

  local Draw_lineCn = function(mode,t,max_num,w,col,alp,st,va,nst,nva)
      local cam_param = Camparam()
      w,col,alp = w or 1, col or 0xffffff, alp or 1
      if not tostring(col):find("table:") then col = {col} end
      if not tostring(alp):find("table:") then alp = {alp} end
      st,va,nst,nva = st or 2000, va or 2500,nst or 0,nva or 1
      if (max_num==nil) then
          max_num = {#t,1}
      elseif not tostring(max_num):find("table:") then
          max_num = {max_num, 1}
      end
      local maxnum = max_num[1]
      local maxcount = max_num[2] or 1
      if maxcount<0 then maxcount=#t end
      maxnum,af = math.modf(maxnum+1)
      af = (math.cos(math.pi*af^2)-1)*-.5
      if not tostring(t[1]):find("table:") then
          local t0={}
          for i=1,#t/3 do
              t0[i]={t[i*3-2],t[i*3-1],t[i*3]}
          end
          t = t0
      end
      --local p1 = t[1] --第二戻り値(線の先端座標)
      if mode==0 then
          for i=1,#t do
              local color = col[(i-1)%(#col)+1]
              local alpha = alp[(i-1)%(#alp)+1]
              local progress = 1
              if maxnum<i then
                  progress = 0
              elseif maxnum==i then
                  progress = af
              end
              n = Draw_line3D(t[i],t[i%#t+1],w,color,alpha,st,va,nil,nil,progress,cam_param)
          end
      elseif (mode==1) then
          for i=1,#t-1 do
              local n = 0
              for j=i+1,#t do
                  local color = col[(i-1)%(#col)+1]
                  local alpha = alp[(i-1)%(#alp)+1]
                  local progress = 1
                  if maxnum<j then
                      progress = 0
                  elseif maxnum==j then
                      progress = af
                  end
                  --local nc,p1 = Draw_line3D(t[i],t[j%#t+1],w,color,alp,st,va,nil,nil,progress)
                  n = n + Draw_line3D(t[i],t[j],w,color,alpha,st,va,nil,nil,progress,cam_param)
                  if (n>=maxcount) then break end
              end
          end
      elseif (mode==2) then
          local n = 1
          local tmp = {}
          for i=1,#t-1 do
              local jn=0
              for j=i+1,#t do
                  local l = Lensq(t[i],t[j])^.5
                  if (jn>=maxcount) then break end
                  if l>st and l<va then
                      if alp[i]*alp[j]>0 then
                          tmp[n] = {t[i], t[j] ,l=l,col = col[i],alp = alp[i]*alp[j]}
                          jn = jn+1
                          n = n+1
                      end
                  end
              end
          end
          table.sort(tmp,function(a,b) return (a.l < b.l) end)
          n = 0
          for i=1,#tmp do
              -- local color = col[(i-1)%(#col)+1]
              -- local alpha = alp[(i-1)%(#alp)+1]
              local progress = 1
              if maxnum<i then
                  progress = 0
              elseif maxnum==i then
                  progress = af
              end
              --local nc,p1 = Draw_line3D(t[i],t[j%#t+1],w,color,alp,st,va,nil,nil,progress)
              n = n + Draw_line3D(tmp[i][1],tmp[i][2],w,tmp[i].col,tmp[i].alp,st,va,nil,nil,progress,cam_param)
              --if (n>=maxcount) then break end
          end


      elseif (mode==3) then
          -- local maxcount,af = math.modf(maxcount)

          for i=1,#t-1 do
              local n = 0
              for j=i,#t do
                  local progress = 1
                  local color = col[n%(#col)+1]
                  local alpha = alp[n%(#alp)+1]
                  --if maxcount<n then
                  --	progress = 0
                  --elseif j==maxcount then
                  --	progress = af
                  --end
                  if Length(Sub(t[i],t[j%#t+1]))<=va then
                      n = n + Draw_line3D(t[i],t[j%#t+1],w,color,alpha,st,va,nil,nil,progress,cam_param)
                  end
                  if (n>=maxnum) then break end
              end
          end
      end
      return p1
  end

  local Draw_lineCn2 = function(mode,t,max_num,w,col,alp,st,va,nst,nva)
  		local cam_param = Camparam()
  		w,col,alp = w or 1, col or 0xffffff, alp or 1
  		st,va,nst,nva = st or 2000, va or 2500,nst or 0,nva or 1
  		if (max_num==nil) then
  			max_num = {#t,1}
  		elseif not tostring(max_num):find("table:") then
  			max_num = {max_num, 1}
  		end
  		local maxnum = max_num[1]
  		local maxcount = max_num[2] or 1
  		if maxcount<0 then maxcount=#t end
  		maxnum,af = math.modf(maxnum+1)
  		af = (math.cos(math.pi*af^2)-1)*-.5
        --[[
  		if not tostring(t[1]):find("table:") then
  			local t0={}
  			for i=1,#t/3 do
  				t0[i]={t[i*3-2],t[i*3-1],t[i*3]}
  			end
  			t = t0
  		end
        ]]
  		--local p1 = t[1] --第二戻り値(線の先端座標)
  		if mode==0 then
  			for i=1,#t do
  				local color = t[i].col
                local alpha = t[i].alpha*alp
  				local progress = 1
  				if maxnum<i then
  					progress = 0
  				elseif maxnum==i then
  					progress = af
  				end
  				n = Draw_line3D(t[i].pos,t[i%#t+1].pos,w,color,alpha,st,va,nil,nil,progress,cam_param)
  			end
        elseif (mode==1) then
            for i=1,#t-1 do
                local n = 0
                for j=i+1,#t do
                    local color = t[i].col
                    local alpha = t[i].alpha*alp
                    local progress = 1
                    if maxnum<j then
                        progress = 0
                    elseif maxnum==j then
                        progress = af
                    end
                    --local nc,p1 = Draw_line3D(t[i],t[j%#t+1],w,color,alp,st,va,nil,nil,progress)
                    n = n + Draw_line3D(t[i].pos,t[j].pos,w,color,alpha,st,va,nil,nil,progress,cam_param)
                    if (n>=maxcount) then break end
                end
            end
  		elseif (mode==2) then
            local n = 1
            local tmp = {}
                for i=1,#t-1 do
                    local jn=0
                    for j=i+1,#t do
                        if (jn>=maxcount) then break end
                        local l = Lensq(t[i].pos,t[j].pos)^.5
                        if l>st and l<(st+va) then
                            if t[i].alpha*t[j].alpha>0 then
                                tmp[n] = {t[i].pos, t[j].pos,l=l,col = t[i].col,alp = math.min(1,t[i].alpha*t[j].alpha)}
                                jn = jn+1
                                n = n+1
                            end
                        end
                    end
                end
            --table.sort(tmp,function(a,b) return (a.l < b.l) end)
    		n = 0
    		for i=1,#tmp do
    				local progress = 1
    				if maxnum<i then
    					progress = 0
    				elseif maxnum==i then
    					progress = af
    				end
    				n = n + Draw_line3D(tmp[i][1],tmp[i][2],w,tmp[i].col,tmp[i].alp,st,va,nil,nil,progress,cam_param)
    		end


  		elseif (mode==3) then
  			--local maxcount,af = math.modf(maxcount)
            local ptime = os.clock()
            local tmp = {}
            local i=1
            while i<#t-1 do
                for j=i,#t do
                    local l = Lensq(t[i].pos,t[j].pos)^0.5
                    if (l < st+va) then
                        table.insert(tmp,{t[i].pos,t[j].pos,l,col=t[i].col,alp=t[i].alpha*t[j].alpha})
                    end
                end
                table.sort(tmp,function(a,b) return a[3]<b[3] end)
                local count = 0
                for j=1,#tmp do
                    count = count + Draw_line3D(tmp[j][1],tmp[j][2],w,tmp[j].col,tmp[j].alp,st,va,nil,nil,1,cam_param)
                    if (count>=maxcount) then break end
                end
                    i = i + 1
                    if (os.clock() - ptime > 0.11) then break end
            end

            -- for 1,#t-1 do
            --     local n = 0
            --     for j=i,#t do
            --         local progress = 1
            --         local color = t[i].col
            --         local alpha = t[i].alpha
            --         --if maxcount<n then
            --         --	progress = 0
            --         --elseif j==maxcount then
            --         --	progress = af
            --         --end
            --         if Length(Sub(t[i].pos,t[j%#t+1].pos))<=va then
            --             n = n + Draw_line3D(t[i].pos,t[j%#t+1].pos,w,color,alpha,st,va,nil,nil,progress,cam_param)
            --         end
            --         if (n>=maxnum) then break end
            --     end
            -- end
        end
  		return p1
  end

-- easing.luaがすでにロードされているかチェック
-- あればEにeasingがコピーされる
local _,E = true,package.loaded.easing
if not (E) or obj.ease==nil then
    _,E = pcall(require,"easing")
end
local ez = {
    [0]="linear",
    "linear",                                                   -- 1
    "inSine","outSine","inOutSine","outInSine",                 -- 2,3,4,5
    "inQuad","outQuad","inOutQuad","outInQuad",                 -- 6,7,8,9
    "inCubic","outCubic","inOutCubic","outInCubic",             -- 10,11,12,13
    "inQuart","outQuart","inOutQuart","outInQuart",             -- 15,16,17,18
    "inQuint","outQuint","inOutQuint","outInQuint",             -- 19,20,21,22
    "inExpo","outExpo","inOutExpo","outInExpo",                 -- 23,24,25,26
    "inCirc","outCirc","inOutCirc","outInCirc",                 -- 27,28,29,30
    "inElastic","outElastic","inOutElastic","outInElastic",     -- 31,32,33,34
    "inBack","outBack","inOutBack","outInBack",                 -- 36,37,38,39
    "inBounce","outBounce","inOutBounce","outInBounce"          -- 40,41,42,43
}
if (_) then
        local function sel(ease)
            if not tostring(ease):find("[iol]")==1 then
                ease = ez[tonumber(ease)]
            end
            return ease
        end
        --t,b,c,d = 入力値t、戻り値の初期値b,戻り値の増加範囲c,入力値tの増加範囲d
        -- easeingを番号でも指定できるようにしたもの
        obj.tween = function(ease,t,b,c, s,a,p)
            return E[sel(ease)](t,b,c,(d or 1),  s,a,p)
        end

        --tの値の変化範囲(t0～t1)を(v0～v1)に変換する形式にしたもの
        obj.ease = function(ease,t,t0,t1,v0,v1, s,a,p)
            if (t0<t1) then
                t=math.max(t0,math.min(t,t1))
            else
                t=math.max(t1,math.min(t,t0))
            end
            return E[sel(ease)]((t-t0),v0,(v1-v0),(t1-t0),s,a,p)
        end
    -- else
        -- easeinout
        --ease = math.min(5,tonumber(ease))
        --Tween = function(t,ease) return math.pow(t*t*(3-2*t),ease) end
end

local Depthfx = function(
	pos,		-- オブジェクト座標{x,y,z}
	focalpoint,	-- 焦点の前後
	startfade,	-- フェード開始位置
	vanish,		-- フェード開始からの消滅距離
	near_startfade, -- フェード開始位置(焦点より手前)
	near_vanish,	-- フェード開始からの消滅距離 (焦点より手前)
	focusmode,	-- trueで焦点を目標点に固定
    blur,		-- ﾌﾞﾗｰ強度
    aspect,     --
	blur_mode,	--0 or nil = ぼかし、1=レンズブラー
	alpha,		-- 1で透明度変化
	cam_param,  -- Camparamの戻り値をあらかじめ入れる用
	depth_map  -- 既に使用しているDepthfxの戻り値、エフェクトのみ、計算しない
	)
	pos = pos or {obj.x+obj.ox,obj.y+obj.oy,obj.z+obj.oz}
	focalpoint,startfade,vanish = focalpoint or 0,startfade or 100,vanish or 2500
	near_startfade,near_vanish = near_startfade or startfade,near_vanish or vanish
	alpha = alpha or 0
    aspect = aspect or 0
    local nvanish = 100
	local D,depth,pd,nD=0,0,0,1
	if not depth_map then
		local cam = cam_param --or obj.getoption("camera_param")
		local e = cam.eye --or {c.tx-c.x, c.ty-c.y, c.tz-c.z}
		local d = cam.l --or (e[1]*e[1]+e[2]*e[2]+e[3]*e[3])^.5
		local n = cam.n --or Scale(e,1/l)
		local pd = (focusmode) and (d+focalpoint) or (1024+focalpoint)
		local pl  = Scale(n,pd)
		local pv = {pos[1]-cam.x, pos[2]-cam.y, pos[3]-cam.z}
		local depth  = Dot({pv[1]-pl[1],pv[2]-pl[2],pv[3]-pl[3]},n)
        local npl = Scale(n,nvanish)
        local ndepth = Dot({pv[1]-npl[1],pv[2]-npl[2],pv[3]-npl[3]},n)
        if ndepth<0 then
            nD = math.abs(ndepth)-nvanish
            nD = nD<0 and 0 or nD
            nD = nD>nvanish and nvanish or nD
            nD = (1-nD/nvanish)
        end
		if depth<0 then
            startfade,vanish = near_startfade,near_vanish
        end
		D = math.abs(depth)-startfade
		D = D<0 and 0 or D
		D = D>vanish and vanish or D
		D = (1-D/vanish)

	else
		D,depth,pd,nD = depth_map[1],depth_map[2],depth_map[3],depth_map[4]
	end
	if alpha==1 then obj.alpha = obj.alpha*D end
	if (obj.alpha>0) then
	    if blur>0 then
	        if (blur_mode==1) then
	            obj.effect("レンズブラー","サイズ固定",0,"範囲",blur*(1-D))
	        else
	            obj.effect("ぼかし","サイズ固定",0,"範囲",blur*(1-D),"縦横比",aspect)
	        end
	    end
	end
	return {D,depth,pd,nD}
end



local Vector = {
    Sub    = Sub,
    Add    = Add,
    Scale  = Scale,
	Unm    = Unm,
    Len    = Len,
    Lensq  = Lensq,
    Length = Length,
    Norm   = Normalize,
    Normalize = Normalize,
    Cross  = Cross,
    Dot    = Dot,
    Mul    = Mul,
    Div    = Div,
	Slide  = Slide,
	Reflect= Reflect,


    Dist_p_line    = Dist_p_line,
    Dist_p_segment = Dist_p_segment,
    Pos_p_on_plane = Pos_p_on_plane,
    Pos_p_on_poly  = Pos_p_on_poly,
    Pos_p_on_line                  = Pos_p_on_line,
    Pos_p_on_segment               = Pos_p_on_segment,
    Pos_plane_intersection_segment = Pos_plane_intersection_segment,
    Pos_intersection_2line         = Pos_intersection_2line,

    Vec_reflect = Vec_reflect,
    Norm_cameraplane = Norm_cameraplane,
    Norm_surface = Norm_surface
}

getcolortools = {
    Vector = Vector,
	Set = Set,
    Linear = Linear,
	LinearN = LinearN,
    TBL = TBL,
    Tbl = Tbl,
		Splt = Splt,
    GL = GL,
    GL2= GL2,
    Param = Param,
    Camparam = Camparam,
    Progress = Progress,
    Swap_table = Swap_table,
    Swap_i_table = Swap_i_table,
    Swap_col = Swap_col,
    Rot = Rot,
    Rot_c = Rot_c,
    Rot_rpy = Rot_rpy,
    Rot_v = Rot_v,
    Co_spherical = Co_spherical,
    Co_torus = Co_torus,
    Draw_line = Draw_line,
    Draw_line3D = Draw_line3D,
    Draw_line3D2 = Draw_line3D2,
    Draw_lineCn = Draw_lineCn,
    Draw_lineCn2 = Draw_lineCn2,
    Depthfx = Depthfx
}
--return false
