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
      // TODO: TRACK_CALLS define checks should be replaces with checks in the code generator; use this flag
      val enableTimerCallStacks = true
      // TODO: is MPI is enabled, average the timings over the number of threads using the given strategy
      // Comment: Meaningless, strategy is defined on test manager, not in timer.
      val timerCollectionOperator = "MEAN" // "MIN", "MAX"
      // TODO: fix the compile warnings
      // TODO: this has to work with and without std::chrono
      val chronoIsAvailable = true
      val windowsSystem = true

      val writerHeader = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h")
      val writerSource = PrettyprintingManager.getPrinter(s"Util/Stopwatch.cpp")

      writerSource.addInternalDependency(s"Util/Stopwatch.h")

      writerHeader << ("""
#include <string>
#include <vector>
#include <algorithm>
        
inline void assert(bool x)
{
#if _DEBUG
	if(!x) abort();
#endif
}
	""");

      if (chronoIsAvailable) {
        writerHeader << ("""
#include <chrono>
using namespace std::chrono;
using std::chrono::nanoseconds;
#define TIMER_ZERO nanoseconds::zero() //zero in current time format
#define TIMER_NOW high_resolution_clock::now() //now in current time format
#define TIMER_TIME_STORE_TYPE nanoseconds //current time format
#define TIMER_TIME_TIMEPOINT_TYPE high_resolution_clock::time_point //timepoint in current time format
#define TIMER_GET_TIME_STORE(dif) duration_cast<nanoseconds>(dif)  //cast from time interval to current time format
#define TIMER_GET_TIME_MILI(dif) (duration_cast<nanoseconds>(dif).count())*1e-6 //cast from time interval to milliseconds
	    """);
      } else {
        if (windowsSystem) {
          writerHeader << ("""
#include <time.h>
           """);
        } else {
          writerHeader << ("""
#include <sys/time.h>
           """);
        }
        writerHeader << ("""
#include <sys/types.h>
          """);

        if (windowsSystem) {
          writerHeader << ("""
inline double time_now() //millisec. hack
{
	clock_t t = clock();
	return (((double)t)/(CLOCKS_PER_SEC/1000.0));
};
           """);
        } else {
          writerHeader << ("""
inline double time_now() //millisecs. unix
{
	timeval timePoint;
	gettimeofday(&timePoint, NULL);
	return (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;	
};
           """);
        }

        writerHeader << ("""
#define TIMER_NOW time_now()
#define TIMER_ZERO 0.0
#define TIMER_TIME_STORE_TYPE double //assumption - primary time in milliseconds
#define TIMER_TIME_TIMEPOINT_TYPE double 
#define TIMER_GET_TIME_STORE(dif) dif
#define TIMER_GET_TIME_MILI(dif) dif   
          """);
      }

      writerHeader << (""" 
class TimerWrapper
{
       """);

      if (enableTimerCallStacks) {
        writerHeader << (""" 
friend class CallTracker;
       """);
      }

      writerHeader << ("""
public:
	TimerWrapper(void);
	TimerWrapper(std::string name);
	~TimerWrapper(void);

	void Start();
	void Stop();

	double getTotalTimeInMilliSec();
	double getLastTimeInMilliSec();
	double getMeanTimeInMilliSec();


	std::string GetName();
private:
	unsigned int _entries;
	unsigned int _total_entries;
	TIMER_TIME_TIMEPOINT_TYPE _time_start;
	TIMER_TIME_STORE_TYPE _mcs_global;
	TIMER_TIME_STORE_TYPE _mcs_local;
	
	std::string _name;
	void UniqueInit();
};
    """);

      if (enableTimerCallStacks) {
        writerHeader << ("""
    class CallEntity
{
friend class CallTracker;
public:
	CallEntity()
	{
		_local_mcs = TIMER_ZERO;
		_parent = nullptr;
		_binded_timer = nullptr;
		_stoped = true;
	};

	CallEntity(CallEntity* parent, std::string name, TimerWrapper* bt)
	{
		_parent = parent;
		_binded_timer = bt;
		_local_mcs = TIMER_ZERO;
		_name = name;
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


private:
	CallEntity* _parent;
	std::vector<CallEntity*> _childs;
	TIMER_TIME_TIMEPOINT_TYPE _call_entity_time_start;
	TIMER_TIME_STORE_TYPE _local_mcs;
	std::string _name;
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
			_new_call = new CallEntity(*s_head, timer->_name, timer);
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

	static void ClearCallStack()
	{
		Clear(_root);
	}

private:
	static CallEntity* _root;
	static TimerWrapper _root_timer;
    
	static CallEntity** GetHead()//current timer we are in
	{
		static CallEntity** head = &_root;
		return head;
	}

	static void print_with_indent(int indent, const char * string, double dur)
	{
		printf("%*s" "%s: %f ms\n", indent, " ", string, dur); 
	}

	static void Print( int displacement, CallEntity* root )
	{
		if( root->_stoped )
		{
			double dur = TIMER_GET_TIME_MILI(root->_local_mcs);
			print_with_indent(displacement, (root->_name).c_str(), dur);

			for( int i = 0; i < root->_childs.size(); i++ )
			{
				Print(displacement+1,(root->_childs)[i]);
			}
		}
		else
		{
			print_with_indent(displacement, "ERROR: TIMER HAS NOT BEEN STOPPED!",0);
		}
	}

	static void Clear( CallEntity* root )
	{
		for( int i = 0; i < root->_childs.size(); i++ )
		{
			Clear((root->_childs)[i]);
		}
		delete root;
	}

}; """);
      }

      writerSource << ("""         
TimerWrapper::TimerWrapper(void)
{	
	_name = "Unset name";
	UniqueInit();
}

TimerWrapper::TimerWrapper(std::string name)
{
	_name = name;
	UniqueInit();
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
	}
""");

      if (enableTimerCallStacks) {
        writerSource << ("""  
	CallTracker::StartTimer(this);
	    """);
      }

      writerSource << ("""  
	++_entries;
	++_total_entries;
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

""");
      if (enableTimerCallStacks) {
        writerSource << ("""  
	CallTracker::StopTimer(this);;
	    """);
      }

      writerSource << ("""  
	--_entries;
}

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
	return TIMER_GET_TIME_MILI(_mcs_global)/_total_entries;
}



TimerWrapper::~TimerWrapper(void)
{
}
""");
      if (enableTimerCallStacks) {
        writerSource << ("""  
CallEntity* CallTracker::_root = new CallEntity( nullptr, "DUMB", &(CallTracker::_root_timer));
TimerWrapper CallTracker::_root_timer("RootTimer");
	    """);
      }
    }
  }
}