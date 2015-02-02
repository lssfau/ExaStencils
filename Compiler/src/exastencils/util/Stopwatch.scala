package exastencils.util

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Stopwatch() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    if (!Knowledge.l3tmp_genAdvancedTimers) {
      val writer = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h")

      if ("MSVC" == Knowledge.targetCompiler || "GCC" == Knowledge.targetCompiler) {
        writer.addExternalDependency("chrono")
        writer << ("""
class StopWatch
{
public:
	StopWatch ()
		: lastTime(std::chrono::high_resolution_clock::now())
	{}

	~StopWatch ()
	{}

	void	reset ()
	{ lastTime = std::chrono::high_resolution_clock::now(); }

	float	getTimeInMilliSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e6);
	}

	float	getTimeInSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e9);
	}

	float	getTimeInSecAndReset ()
	{
		float timeStep = getTimeInSec();
		reset();
		return timeStep;
	}

	float	getTimeInMilliSecAndReset ()
	{
		float timeStep = getTimeInMilliSec();
		reset();
		return timeStep;
	}

protected:
	std::chrono::high_resolution_clock::time_point	lastTime;		///< stores the initial point of time
};
""")
      } else {
        writer.addExternalDependency("sys/time.h")
        writer.addExternalDependency("sys/types.h")
        writer << ("""
class StopWatch
{
public:
	StopWatch ()
	{ reset(); }

	~StopWatch ()
	{}

	void	reset ()
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		lastTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
	}

	float	getTimeInMilliSec () const
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		double newTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
		return newTime - lastTime;
	}

	float	getTimeInMilliSecAndReset ()
	{
		float timeStep = getTimeInMilliSec();
		reset();
		return timeStep;
	}

protected:
	double		lastTime;		///< stores the initial point of time
};
""")
      }
    } else {
      val writerHeader = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h")
      val writerSource = PrettyprintingManager.getPrinter(s"Util/Stopwatch.cpp")

      writerSource.addInternalDependency(s"Util/Stopwatch.h")

      writerHeader << ("""
#include <string>
#include <vector>
#include <algorithm>
#include <fstream>
#include <sstream>
        
inline void assert(bool x)
{
#if _DEBUG
	if(!x) abort();
#endif
}

template <typename T>
std::string to_string(T value)
{
	std::ostringstream os ;
	os << value ;
	return os.str() ;
}       
        
#define newline '\n'
	""")

      Knowledge.advTimer_timerType match {
        case "Chrono" => writerHeader << ("""
#	include <chrono>
using namespace std::chrono;
using std::chrono::nanoseconds;
#	define TIMER_ZERO nanoseconds::zero() //zero in current time format
#	define TIMER_NOW high_resolution_clock::now() //now in current time format
#	define TIMER_TIME_STORE_TYPE nanoseconds //current time format
#	define TIMER_TIME_TIMEPOINT_TYPE high_resolution_clock::time_point //timepoint in current time format
#	define TIMER_GET_TIME_STORE(dif) duration_cast<nanoseconds>(dif)  //cast from time interval to current time format
#	define TIMER_GET_TIME_MILI(dif) (duration_cast<nanoseconds>(dif).count())*1e-6  //cast from time interval to milliseconds
	    """)

        case "QPC" => writerHeader << ("""
#	include <windows.h>
#	define TIMER_TIME_STORE_TYPE long long //current time format
#	define TIMER_TIME_TIMEPOINT_TYPE long long //timepoint in current time format

inline TIMER_TIME_TIMEPOINT_TYPE milliseconds_now() 
{
	LARGE_INTEGER now;
	QueryPerformanceCounter(&now);
	return (now.QuadPart);
}

inline double to_milisec(TIMER_TIME_STORE_TYPE val)
{
	static LARGE_INTEGER s_frequency;
	static BOOL s_use_qpc = QueryPerformanceFrequency(&s_frequency);
	return val/(s_frequency.QuadPart/1000.0);
}

#	define TIMER_ZERO 0  //zero in current time format
#	define TIMER_NOW milliseconds_now() //now in current time format

#	define TIMER_GET_TIME_STORE(dif) dif //cast from time interval to current time format
#	define TIMER_GET_TIME_MILI(dif) to_milisec(dif) //cast from time interval to milliseconds
    	""")

        case "WIN_TIME" => writerHeader << ("""
#   include <time.h>
#	include <sys/types.h>
#	define TIMER_ZERO 0.0
#	define TIMER_TIME_STORE_TYPE double //assumption - primary time in milliseconds
#	define TIMER_TIME_TIMEPOINT_TYPE double 

inline TIMER_TIME_TIMEPOINT_TYPE time_now() //millisec. hack
{
	clock_t t = clock();
	return (((TIMER_TIME_TIMEPOINT_TYPE)t)/(CLOCKS_PER_SEC* 1e-3));
};

#	define TIMER_NOW time_now()

#	define TIMER_GET_TIME_STORE(dif) dif
#	define TIMER_GET_TIME_MILI(dif) dif
    	""")

        case "UNIX_TIME" => writerHeader << ("""
#	include <sys/time.h>
#	include <sys/types.h>
#	define TIMER_TIME_STORE_TYPE double //assumption - primary time in milliseconds
#	define TIMER_TIME_TIMEPOINT_TYPE double
inline TIMER_TIME_TIMEPOINT_TYPE time_now() //millisecs. unix
{
	timeval timePoint;
	gettimeofday(&timePoint, NULL);
	return (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
}
#	define TIMER_NOW time_now()
#	define TIMER_ZERO 0.0
#	define TIMER_GET_TIME_STORE(dif) dif
#	define TIMER_GET_TIME_MILI(dif) dif
    	""")

        case "MPI_TIME" => writerHeader << ("""
#	include <mpi.h>
#	define TIMER_ZERO 0.0
#	define TIMER_NOW MPI_Wtime()
#	define TIMER_TIME_STORE_TYPE double //assumption - primary time in milliseconds
#	define TIMER_TIME_TIMEPOINT_TYPE double 
#	define TIMER_GET_TIME_STORE(dif) dif
#	define TIMER_GET_TIME_MILI(dif) dif*1e3          
    	""")

        case "WINDOWS_RDSC" => writerHeader << ("""
#	define PREQ 2300000000
#	define TIMER_TIME_STORE_TYPE long long
#	define TIMER_TIME_TIMEPOINT_TYPE long long 

inline TIMER_TIME_TIMEPOINT_TYPE now()
{
	return __rdtsc();
}
#	define TIMER_ZERO 0
#	define TIMER_NOW now()
#	define TIMER_GET_TIME_STORE(dif) dif
#	define TIMER_GET_TIME_MILI(dif) dif/(PREQ/1000.0)

#	pragma intrinsic(__rdtsc)
inline TIMER_TIME_STORE_TYPE rdtsc() {
	return __rdtsc();
}          
	    	""")

        case "RDSC" => writerHeader << ("""
#	define PREQ 2300000000
#	define TIMER_TIME_STORE_TYPE long long
#	define TIMER_TIME_TIMEPOINT_TYPE long long 

inline TIMER_TIME_TIMEPOINT_TYPE now()
{
	return __rdtsc();
}
#	define TIMER_ZERO 0
#	define TIMER_NOW now()
#	define TIMER_GET_TIME_STORE(dif) dif
#	define TIMER_GET_TIME_MILI(dif) dif/(PREQ/1000.0)

#include <stdint.h>
extern __inline__ TIMER_TIME_STORE_TYPE rdtsc() {
TIMER_TIME_STORE_TYPE x;
__asm__ volatile ("rdtsc\n\tshl $32, %%rdx\n\tor %%rdx, %%rax" : "=a" (x) : : "rdx");
return x;
}
	    	""")

      }

      writerHeader << (""" 
	class TimerWrapper
	{
	       """)

      if (Knowledge.advTimer_enableCallStacks) {
        writerHeader << (""" 
	friend class CallTracker;
	       """)
      }

      writerHeader << ("""
	public:
		TimerWrapper(void);
		TimerWrapper(std::string name);
		~TimerWrapper(void);
	
		void Start();
		void Stop();
	    
	    void Reset();
	
		double getTotalTimeInMilliSec();
		double getLastTimeInMilliSec();
		double getMeanTimeInMilliSec();
	    
	    std::string ToCSV();//name;total;mean
		static void PrintAllTimersGlobal();//prints CSV of all timers from all nodes
		static void PrintAllTimersToFileGlobal( std::string name );//prints CSV of all timers from all nodes to #filename
	
	
		std::string GetName();
	    std::string _name;
	private:
		unsigned int _entries;
		unsigned int _total_entries;
		TIMER_TIME_TIMEPOINT_TYPE _time_start;
		TIMER_TIME_STORE_TYPE _mcs_global;
		TIMER_TIME_STORE_TYPE _mcs_local;
		
		//std::string _name;
		void UniqueInit();
		static void GatherAllTimersCSV(std::vector<std::string>& out_data);
		static std::vector<TimerWrapper*> _all_timers;
	};
    """)

      if (Knowledge.advTimer_enableCallStacks) {
        writerHeader << ("""
    

class CallEntity
{
friend class CallTracker;
public:

	CallEntity(CallEntity* parent, TimerWrapper* bt)
	{
		_parent = parent;
		_binded_timer = bt;
		_local_mcs = TIMER_ZERO;
		_stoped = true;
	};

	~CallEntity()
	{

	};

	void Start()
	{
		_call_entity_time_start = TIMER_NOW;
		_stoped = false;
	};

	void Stop()
	{
		_local_mcs += TIMER_GET_TIME_STORE(TIMER_NOW - _call_entity_time_start);
		_stoped = true;
	};

	bool CheckHasTimer(TimerWrapper* bt, CallEntity** ent)
	{
		//std::vector<CallEntity*>::iterator it = find (_childs.begin(), _childs.end(), bt);
		for ( int i =0; i < _childs.size(); i++ )
		{
			if ( (_childs[i])->_binded_timer == bt )
			{
				*ent = (_childs[i]);
				return true;
			}
		}
		return false;//it == _childs.end();
	}

	void AddChild(CallEntity* child)
	{
		_childs.push_back(child);

		//erase doubles
		/*std::sort(_childs.begin(), _childs.end()); 
		auto last = std::unique(_childs.begin(), _childs.end());
		_childs.erase(last, _childs.end());
		*/
	};

	std::string ToCSV()//name;time
	{
		return _binded_timer->GetName() +';'+ to_string(TIMER_GET_TIME_MILI(_local_mcs))+';';
	}


private:
	CallEntity();

	CallEntity* _parent;
	std::vector<CallEntity*> _childs;
	TIMER_TIME_TIMEPOINT_TYPE _call_entity_time_start;
	TIMER_TIME_STORE_TYPE _local_mcs;
	CallEntity(CallEntity&);
	bool _stoped;
	TimerWrapper* _binded_timer;
	
};

class CallTracker
{
	friend class TimerWrapper;

private:

	static void StartTimer(TimerWrapper* timer)
	{
		CallEntity** s_head = GetHead();//current stack head
		CallEntity* _new_call = nullptr;
		if ( (*s_head)->CheckHasTimer(timer, &_new_call) )//To have measured every entry separately, comment this "if"
		{
			_new_call->Start();
			*s_head = _new_call; 
		}
		else
		{
			_new_call = new CallEntity(*s_head, timer);
			_new_call->Start();
			(*s_head)->AddChild(_new_call);

			*s_head = _new_call;  
		}
		  
	}

	static void StopTimer( TimerWrapper* timer )
	{		
		CallEntity** s_head = GetHead();
		assert(( (*s_head)->_binded_timer) == timer );
		(*s_head)->Stop();
		*s_head = ((*s_head)->_parent);
	}


public:

	static void PrintCallStack()
	{
		printf("\nCall stack:\n");
		for( int i = 0; i < _root->_childs.size(); i++ )
		{
			Print(0,(_root->_childs)[i]);
		}
	}

	static void PrintCallStackToFileML( const std::string& nm )
	{
		std::ofstream out(nm);
		char newl = newline;
		std::vector<std::string> _data = GetCallStackAsCSVML(_root);

		for( int i = 0; i < _data.size(); i++ )
		{
			out.write( _data[i].c_str(), sizeof(char)*_data[i].size() );
			out.write( &newl, sizeof(char) );
		}
		out.close();
	}

	static void PrintCallStackToFile( const std::string& nm )
	{
		std::ofstream out(nm);

		std::string _data = GetCallStackAsCSV(_root);
		out.write( _data.c_str(), sizeof(char)*_data.size() );

		out.close();
	}

	static void PrintCallStackToFileGlobal( const std::string& nm )
	{
		std::vector<std::string> timer_data;
		int rank = GetCallStackAsCSVGlobal(timer_data);

		if (rank == 0)
		{
			std::ofstream out(nm);
			char newl = newline;
			for( int i = 0; i < timer_data.size(); i++ )
			{
				out.write( timer_data[i].c_str(), sizeof(char)*timer_data[i].size() );
				out.write( &newl, sizeof(char) );
			}
			out.close();
		}
	}

	

	static void ClearCallStack()
	{
		Clear(_root);
	}

private:
	static TimerWrapper _root_timer;
	static CallEntity* _root;
	
	static CallEntity** GetHead()//current timer we are in
	{
		static CallEntity** head = &_root;
		return head;
	}

	static void print_with_indent(int indent, std::string str)
	{
		printf("%*s" "%s ms\n", indent+1, " ", str.c_str());
	}

	static void Print( int displacement, CallEntity* root )
	{
		if( root->_stoped )
		{
			print_with_indent(displacement, root->ToCSV());

			for( int i = 0; i < root->_childs.size(); i++ )
			{
				Print(displacement+1,(root->_childs)[i]);
			}
		}
		else
		{
			print_with_indent(displacement, "ERROR: TIMER HAS NOT BEEN STOPPED!");
		}
	}

	static std::vector<std::string> GetCallStackAsCSVIntend( int displacement, CallEntity* root )
	{
		std::vector<std::string> _data;
		
		if( root->_stoped )
		{
			_data.push_back(to_string(displacement)+";" + root->ToCSV() );

			for( int i = 0; i < root->_childs.size(); i++ )
			{
				std::vector<std::string> _n_data = GetCallStackAsCSVIntend( displacement+1, (root->_childs)[i]);
				_data.insert( _data.end(), _n_data.begin(), _n_data.end() );
			}
		}
		else
		{
			_data.push_back(to_string(displacement)+";" + root->_binded_timer->GetName() +  ";ERROR: TIMER HAS NOT BEEN STOPPED!" );
		}
		return _data;
	}

	static std::vector<std::string> GetCallStackAsCSVML( CallEntity* root ) 
	{
		std::vector<std::string> _data;
		for( int i = 0; i < _root->_childs.size(); i++ )
		{
			std::vector<std::string> _l_data = GetCallStackAsCSVIntend( 0, (root->_childs)[i]);
			_data.insert( _data.end(), _l_data.begin(), _l_data.end() );
		}
		
		return _data;
	}

	static std::string GetCallStackAsCSV( CallEntity* root ) 
	{
		std::string _data;
		for( int i = 0; i < _root->_childs.size(); i++ )
		{
			std::vector<std::string> _l_data = GetCallStackAsCSVIntend( 0, (root->_childs)[i]);
			for ( int i = 0; i < _l_data.size(); i++ )
			{
				_data += _l_data[i];
			}
		}
		return _data;
	}

	static int GetCallStackAsCSVGlobal( std::vector<std::string>& out_data);

	static void Clear( CallEntity* root )
	{
		for( int i = 0; i < root->_childs.size(); i++ )
		{
			Clear((root->_childs)[i]);
		}
		delete root;
	}
};
 """)
      }

      if (Knowledge.useMPI) {
        writerSource << ("""
#include <mpi.h>
    """)
      }
      writerSource << (""" 	
TimerWrapper::TimerWrapper(void)
{	
	_name = "Unset name";
	UniqueInit();
	_all_timers.push_back(this);
}

TimerWrapper::TimerWrapper(std::string name)
{
	_name = name;
	UniqueInit();
	_all_timers.push_back(this);
}

void TimerWrapper::UniqueInit()
{
	_entries = 0;
	_total_entries = 0;
	_mcs_global = TIMER_ZERO;
	_mcs_local = TIMER_ZERO;
}

void TimerWrapper::Start()
{
	if (!_entries)
	{
		_time_start = TIMER_NOW;
		_mcs_local = TIMER_ZERO;

		++_total_entries;
	}
""")

      if (Knowledge.advTimer_enableCallStacks) {
        writerSource << ("""  
	CallTracker::StartTimer(this);
	    """)
      }

      writerSource << ("""  
	++_entries;	
}
        
void TimerWrapper::Reset()
{
	UniqueInit();
}

void TimerWrapper::Stop()
{
	if ( _entries == 0 )
	{
		_name = "Invalid"; //Check your code, you stop before start.
	}
	if (_entries == 1) // if you don't properly stop all calls, _ns_local will be 0.
	{
		_mcs_local = TIMER_GET_TIME_STORE(TIMER_NOW -_time_start);
		_mcs_global += _mcs_local;
	}

""")
      if (Knowledge.advTimer_enableCallStacks) {
        writerSource << ("""  
	CallTracker::StopTimer(this);;
	    """)
      }

      writerSource << ("""  
	--_entries;
}

void TimerWrapper::GatherAllTimersCSV( std::vector<std::string>& out_data)
{
    """)

      if (Knowledge.useMPI) {
        writerSource << ("""
	  int rank, N;
	MPI_Comm_size(MPI_COMM_WORLD, &N);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	if ( rank != 0 )
	{
		//calc space:
		std::string timer_datas;
		for( int i = 0; i < _all_timers.size(); i++ )
		{
			timer_datas += _all_timers[i]->ToCSV();
		}
		MPI_Send((void*)timer_datas.c_str(), (int)timer_datas.size(), MPI_CHAR, 0, 0, MPI_COMM_WORLD);
	}
	else
	{
		
	  """)
      }
      writerSource << ("""
		//this node:
		std::string timer_datas;
		for( int i = 0; i < _all_timers.size(); i++ )
		{
			timer_datas += _all_timers[i]->ToCSV();
		}

		out_data.push_back(timer_datas);
		
	   """)
      if (Knowledge.useMPI) {
        writerSource << ("""
	    //other
		MPI_Status status;

		for( int i = 1; i < N; i++ )
		{
			int size = 0;
			MPI_Probe(i, 0, MPI_COMM_WORLD, &status);
			MPI_Get_count(&status, MPI_CHAR, &size);

			char* arr = new char[size];
			MPI_Recv(arr, size, MPI_CHAR, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

			std::string timer_datas(arr);
			delete[] arr;
			timer_datas.resize(size);
			out_data.push_back(timer_datas);		
		}
	}
	   """)
      }
      writerSource << ("""
}

void TimerWrapper::PrintAllTimersGlobal()
{
	printf("Timer data from all nodes:\n");

	std::vector<std::string> timer_data;
	GatherAllTimersCSV(timer_data);
""")

      if (Knowledge.useMPI) {
        writerSource << ("""

    int rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	if ( rank == 0 )
	{
	""")
      }
      writerSource << ("""
		for( int i = 0; i < timer_data.size(); i++ )
		{
			printf("Node #%d: %s\n", i, (timer_data[i]).c_str());
		}
	    """)
      if (Knowledge.useMPI)
        writerSource << ("""
	}
	""")

      writerSource << ("""
}

void TimerWrapper::PrintAllTimersToFileGlobal( std::string name )
{
	std::vector<std::string> timer_data;
	GatherAllTimersCSV(timer_data);
 """)
      if (Knowledge.useMPI)
        writerSource << ("""
	int rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	if ( rank == 0 )
	{
	     """)

      writerSource << ("""
	
		std::ofstream out(name);
		char newl = newline;
		for( int i = 0; i < timer_data.size(); i++ )
		{
			out.write( timer_data[i].c_str(), sizeof(char)*timer_data[i].size() );
			out.write( &newl, sizeof(char) );
		}
		out.close();
 """)
      if (Knowledge.useMPI) {
        writerSource << ("""
	}
	""")
      }
      writerSource << ("""
}
	""")

      writerSource << ("""

std::string TimerWrapper::GetName()
{
	return _name;
}

double TimerWrapper::getLastTimeInMilliSec()
{
	return TIMER_GET_TIME_MILI(_mcs_local);
}

double TimerWrapper::getTotalTimeInMilliSec()
{
	return TIMER_GET_TIME_MILI(_mcs_global);
}


double TimerWrapper::getMeanTimeInMilliSec()
{
	return _total_entries? TIMER_GET_TIME_MILI(_mcs_global)/_total_entries : 0.0;
}


std::string TimerWrapper::ToCSV()//name;time
{
	return _name +';'+ to_string(getTotalTimeInMilliSec())+';'+ to_string(getMeanTimeInMilliSec())+';';
}
    

TimerWrapper::~TimerWrapper(void)
{
    if ( _all_timers.size() )
	{
		auto tmp = std::find(_all_timers.begin(), _all_timers.end(), this);
		if ( tmp !=_all_timers.end() )
			_all_timers.erase(tmp);
	}
}
    
std::vector<TimerWrapper*> TimerWrapper::_all_timers;
""")
      if (Knowledge.advTimer_enableCallStacks) {
        writerSource << ("""
TimerWrapper CallTracker::_root_timer("RootTimer");
CallEntity* CallTracker::_root = new CallEntity( nullptr, &(CallTracker::_root_timer));

	    
	    
int CallTracker::GetCallStackAsCSVGlobal( std::vector<std::string>& out_data)
{

	""")

        if (Knowledge.useMPI) {
          writerSource << ("""	    
	int rank, N;
	MPI_Comm_size(MPI_COMM_WORLD, &N);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	if ( rank != 0 )
	{
		//calc space:
		std::string callstack_datas = GetCallStackAsCSV(_root);
		MPI_Send((void*)callstack_datas.c_str(), (int)callstack_datas.size(), MPI_CHAR, 0, 0, MPI_COMM_WORLD);
	}
	else
	{
	   
		
	""")
        }
        writerSource << ("""

		//this node:
		std::string timer_datas = GetCallStackAsCSV(_root);
		
		out_data.push_back(timer_datas);

	""")
        if (Knowledge.useMPI) {
          writerSource << ("""

		//other
		MPI_Status status;

		for( int i = 1; i < N; i++ )
		{
			int size = 0;
			MPI_Probe(i, 0, MPI_COMM_WORLD, &status);
			MPI_Get_count(&status, MPI_CHAR, &size);

			char* arr = new char[size];
			MPI_Recv(arr, size, MPI_CHAR, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

			std::string timer_datas(arr);
			delete[] arr;
			timer_datas.resize(size);

			out_data.push_back(timer_datas);		
		}

	}
	return rank;
}

	    """)
        } else {
          writerSource << ("""
    return 0;
}
	    """)
        }
      }
    }
  }
}