package exastencils.util

import exastencils.datastructures._
import exastencils.config._
import exastencils.prettyprinting._

case class Stopwatch() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    if (Knowledge.experimental_timerEnableCallStacks) {
      CallEntity().printToFile()
      CallTracker().printToFile()
    }

    val writerHeader = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h")
    writerHeader.addExternalDependency("string")

    Knowledge.timer_type match {
      case "Chrono" => writerHeader.addExternalDependency("chrono")
      case "QPC"    => writerHeader.addExternalDependency("windows.h")
      case "WIN_TIME" =>
        writerHeader.addExternalDependency("time.h"); writerHeader.addExternalDependency("sys/types.h")
      case "UNIX_TIME" =>
        writerHeader.addExternalDependency("sys/time.h"); writerHeader.addExternalDependency("sys/types.h")
      case "MPI_TIME"     => writerHeader.addExternalDependency("mpi.h")
      case "WINDOWS_RDSC" =>
      case "RDSC"         => writerHeader.addExternalDependency("stdint.h")
    }

    var timerDurationType = ""
    var timerTimepointType = ""

    Knowledge.timer_type match {
      case "Chrono" =>
        timerDurationType = "std::chrono::nanoseconds"; timerTimepointType = "std::chrono::high_resolution_clock::time_point"
      case "QPC" =>
        timerDurationType = "long long"; timerTimepointType = "long long"
      case "WIN_TIME" =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "UNIX_TIME" =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "MPI_TIME" =>
        timerDurationType = "double"; timerTimepointType = "double"
      case "WINDOWS_RDSC" =>
        timerDurationType = "long long"; timerTimepointType = "long long"
      case "RDSC" => timerDurationType = "long long"; timerTimepointType = "long long"
    }

    writerHeader <<< s"class StopWatch {"
    writerHeader <<< s"public:"
    writerHeader <<< s"\tStopWatch()"
    writerHeader << s"\t\t: numEntries(0), numMeasurements(0), lastTimeMeasured(${TimerDetail_Zero().prettyprint()}), totalTimeMeasured(${TimerDetail_Zero().prettyprint()})\n"
    writerHeader <<< s"\t{}\n"

    writerHeader <<< s"\tstd::string timerName;\n"

    writerHeader <<< s"\tint numEntries;"
    writerHeader <<< s"\tint numMeasurements;"
    writerHeader <<< s"\t$timerTimepointType timerStarted;"
    writerHeader <<< s"\t$timerTimepointType timerEnded;"
    writerHeader <<< s"\t$timerDurationType lastTimeMeasured;"
    writerHeader <<< s"\t$timerDurationType totalTimeMeasured;"
    writerHeader <<< s"};\n"
  }
}

case class CallEntity() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    val writer = PrettyprintingManager.getPrinter(s"Util/CallEntity.h")

    writer << ("""
#include "Stopwatch.h"
#include "CallEntity.h"

class CallEntity
{
public:

	CallEntity(CallEntity* parent, StopWatch* bt)
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

	bool CheckHasTimer(StopWatch* bt, CallEntity** ent)
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
		return _binded_timer->timerName +';'+ to_string(TIMER_GET_TIME_MILI(_local_mcs))+';';
	}

	CallEntity();

	CallEntity* _parent;
	std::vector<CallEntity*> _childs;
	TIMER_TIME_TIMEPOINT_TYPE _call_entity_time_start;
	TIMER_TIME_STORE_TYPE _local_mcs;
	CallEntity(CallEntity&);
	bool _stoped;
	StopWatch* _binded_timer;
	
};
""")
  }
}

case class CallTracker() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(s"Util/CallTracker.h")
    val writerSource = PrettyprintingManager.getPrinter(s"Util/CallTracker.cpp")

    writerHeader << ("""
#include "Stopwatch.h"
class CallTracker
{
public:

	static void StartTimer(StopWatch* timer)
	{
		CallEntity** s_head = GetHead();//current stack head
		CallEntity* _new_call = nullptr;
		if ( (*s_head)->CheckHasTimer(timer, &_new_call) )//To have measured every entry separately, comment this "if"
		{
			startTimer(*_new_call);
			*s_head = _new_call; 
		}
		else
		{
			_new_call = new CallEntity(*s_head, timer);
			startTimer(*_new_call);
			(*s_head)->AddChild(_new_call);

			*s_head = _new_call;  
		}
		  
	}

	static void StopTimer( StopWatch* timer )
	{		
		CallEntity** s_head = GetHead();
		assert(( (*s_head)->_binded_timer) == timer );
		stopTimer(**s_head);
		*s_head = ((*s_head)->_parent);
	}

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

	static StopWatch _root_timer;
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
			_data.push_back(to_string(displacement)+";" + root->_binded_timer->timerName +  ";ERROR: TIMER HAS NOT BEEN STOPPED!" );
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

    writerSource << ("""
StopWatch CallTracker::_root_timer("RootTimer");
CallEntity* CallTracker::_root = new CallEntity( nullptr, &(CallTracker::_root_timer));

int CallTracker::GetCallStackAsCSVGlobal( std::vector<std::string>& out_data)
{

	""")

    if (Knowledge.mpi_enabled) {
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
    if (Knowledge.mpi_enabled) {
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