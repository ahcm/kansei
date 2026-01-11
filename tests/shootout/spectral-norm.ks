fn eval_A(i, j)
  1.0 / ((i+j)*(i+j+1)/2+i+1)
end

fn eval_A_times_u(n, u, au)
   loop n |i|
      au[i]=0.0
      loop n |j|
         au[i] = au[i] + eval_A(i,j) * u[j]
      end
   end
end

fn eval_At_times_u(n, u, au)
   loop n |i|
      au[i] = 0
       loop n |j|
         au[i] = au[i] + eval_A(j,i) * u[j]
      end
   end      
end

fn eval_AtA_times_u(n, u, atAu)
   v = [0.0;n]
   eval_A_times_u(n,u,v)
   eval_At_times_u(n,v,atAu)
end

fn main(n)
   u = [1.0;n]
   v = [0.0;n]
   loop 10 |i|
      eval_AtA_times_u(n,u,v)
      eval_AtA_times_u(n,v,u)   
   end
   vBv = 0
   vv = 0
   loop n |i|
     vBv = u[i] * v[i] + vBv
     vv  = v[i] * v[i] + vv
   end

   use std::Float64
   Float64 = std::Float64
   puts Float64.sqrt(vBv/vv)    
end  

n = program.args[0]
use std::Int64
Int64 = std::Int64
n = Int64::parse(n)
main(n)
