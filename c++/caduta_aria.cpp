//calcola con un Runge-Kutta la traiettoria e le velocità di un proiettile lanciato in un fluido con attrito
//versione con oop del programma par att_static.cpp
#include <iostream>
#include <fstream>
#include <string>

const double k=0.5;
const double m=0.15;
const double g=9.8;

class moto{
public:
	
	moto(){								//fa scegliere se l'utente deve inserire gli input o se deve procedere con i valori di inizializzazione precostruiti
		std::string mod;
		int eval;
		std::cout << "Digitare 0 per auto o 1 per manuale per l inizializzazione" << std::endl;
		std::cin >> eval;
		try{
			switch (eval){
				case 0:
					auto_set();
					break;
				case 1:
					man_set();
					break;
				default:
					throw (eval);
			};
		}
		catch(int err_eval){
			std::cout << "errore: input invalido, hai inserito " << err_eval << std::endl;
			std::cout << "---->termina" << std::endl;
		};
		
	};
	
private:	
 	int np;									
	double tmax;
	double dt1=tmax/np;
	double dt2=dt1/2;
	double x_0;
	double y_0;
	double vx_0;
	double vy_0;
	std::ofstream filexy, filevx, filevy;
		
	void auto_set(){					//valori precostruiti
		np=1000;
		tmax=2;
		dt1=tmax/np;
		x_0=0;
		y_0=0;
		vx_0=2;
		vy_0=4;
	};
	
	void man_set(){													//valori inseriti dall'utente
		std::cout << "MOD MANUALE" << std::endl;
		std::cout << "inserisci numero punti" << std::endl;
		std::cin >> np;
		std::cout << "inserisci tempo massimo (t0=0)" << std::endl;
		std::cin >> tmax;
		dt1=tmax/np;
		std::cout << "inserisci posizione iniziale x_0" << std::endl;
		std::cin >> x_0;
		std::cout << "inserisci posizione iniziale y_0" << std::endl;
		std::cin >> y_0;
		std::cout << "inserisci velocita' iniziale vx_0" << std::endl;
		std::cin >> vx_0;
		std::cout << "inserisci velocita' iniziale vy_0" << std::endl;
		std::cin >> vy_0;	
	};
	
	
	double fx(double t, double vx){							//asse x equazione ridotta a dx/dt = f(t,x)
			return -(k/m)*vx;
	};

	double fy(double t, double vy){							//asse y equazione ridotta a dy/dt = f(t,y)
		return -(k/m)*vy-g;
	};

	double step_v(char ch, double t, double v, double dt1, double dt2){   	//Singolo step Runge-Kutta. Le referenziazioni non servono perché sono tutte soltanto 
		double tmp;															//lette e non cambiate. l'unica cambiata è tmp che però è data in uscita
		double k1,k2,k3,k4;
		if (ch == 'x'){
			k1=dt1*fx(t,v);
			k2=dt1*fx(t+dt2,v+k1/2);
			k3=dt1*fx(t+dt2,v+k2/2);
			k4=dt1*fx(t+dt1,v+k3);
			tmp=v+k1/6+k2/3+k3/3+k4/6;
		}
		else if (ch == 'y'){
			k1=dt1*fy(t,v);
			k2=dt1*fy(t+dt2,v+k1/2);
			k3=dt1*fy(t+dt2,v+k2/2);
			k4=dt1*fy(t+dt1,v+k3);
			tmp=v+k1/6+k2/3+k3/3+k4/6;
		};
		return tmp;
	};

	double* rk(char index, double &v_0, int &np, double &dt1, double &dt2){		//loope per gli step Runge-Kutta
		int i=0; 
		double t=0;
		double* out_array_v=new double[2*np];			/*static*/ 
		double v=v_0;
		while (i<np){
			out_array_v[i]=t;
			out_array_v[i+np]=v;
			v=step_v(index, t, v, dt1, dt2);
			//std::cout << "v= " << v << std::endl;
			t=t+dt1;
			i++;
		};
		return out_array_v;
	};

	void write(int np, double *a, double *b, double *c, double *d, std::ofstream &filexy, std::ofstream &filevx, std::ofstream &filevy, std::string nome){  //scive i dati su file
		int i;
		filexy.open(nome + "xy.dat");		//modificare altrimenti se ho due oggetti il secondo va a sovrascrivere i file di output di quello prima
		filevx.open(nome + "vx.dat");
		filevy.open(nome + "vy.dat");
		for (i=0;i<np;i++){
			filexy << c[i] << " " << d[i] << std::endl;
			filevx << a[i] << " " << a[i+np] << std::endl;
			filevy << b[i] << " " << b[i+np] << std::endl;
		};
		filexy.close();
		filevx.close();
		filevy.close();
	};
public:
	void integrale(std::string nome){	//integra le velocità		/*int &np, double &dt1, double &dt2, double &x_0, double & y_0, double &vx_0, double &vy_0, std::ofstream &filexy, std::ofstream &filevx, std::ofstream &filevy*/
		int i,j;
		double sx,sy;
		double *vxx, *vyy;
		vxx=new double[np];
		vyy=new double[np];
	
		vxx=rk('x',vx_0,np,dt1,dt2);
		vyy=rk('y',vy_0,np,dt1,dt2);
		
		double *x,*y;
		x=new double[np];
		y=new double[np];
		for (i=0;i<np;i++){
			sx=x_0;
			sy=y_0;
			for (j=0;j<i;j++){
				sx=sx+vxx[j+np]*dt1;
				sy=sy+vyy[j+np]*dt1;
			};
			x[i]=sx;
			y[i]=sy;
		};	
		write(np, vxx, vyy, x, y, filexy, filevx, filevy, nome);

	};
	
};

int main(){

	moto proiettile_1;
	proiettile_1.integrale("pr1_");

	std::cout << "FINE" << std::endl;
	std::cin.get();
	return 0;
}; 

