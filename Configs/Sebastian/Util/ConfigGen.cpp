#include <iostream>
#include <sstream>
#include <fstream>

int main(int argc, char** argv)
{
	int numNodesOverall[] = { 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 28672 };

	int numConfigs = 0;

	for (auto numNodes : numNodesOverall) {
		std::stringstream jobScriptName;
		jobScriptName << "runJuQueen_" << numNodes;
		auto jobScript = std::ofstream(jobScriptName.str(), std::ios::binary);

		jobScript << "#@ shell = /bin/bash\n"
			<< "#@ job_name = GENERATED_WEAK_HYBRID_" << numNodes << "\n"
			<< "#@ error = $(job_name).$(jobid).out\n"
			<< "#@ output = $(job_name).$(jobid).out\n"
			<< "#@ environment = COPY_ALL\n"
			<< "#@ notification = always\n"
			<< "#@ notify_user = sebastian.kuckuk@fau.de\n"
			<< "#@ job_type = bluegene\n"
			<< "#@ bg_size = " << numNodes << "\n"
			<< "#@ bg_connectivity = TORUS\n"
			<< "#@ wall_clock_limit = 00:30:00\n"
			<< "#@ queue\n";

		for (int numOMPTotal = 1; numOMPTotal <= 64; numOMPTotal *= 2) {
			int curDim = 0;

			int numPerDim[3] = { 1, 1, 1 };
			{
				int tmp = numNodes * 64;
				//tmp /= numOMPTotal;

				while (tmp > 1) {
					if (0 == tmp % 2) {
						numPerDim[curDim % 3] *= 2;
						tmp /= 2;
					} else {
						numPerDim[curDim % 3] *= tmp;
						tmp /= tmp;
					}

					++curDim;
				}
			}

			// add OMP
			int numOMP[3] = { 1, 1, 1 };
			{
				--curDim;
				int tmp = numOMPTotal;
				while (tmp > 1) {
					numOMP[curDim % 3] *= 2;
					numPerDim[curDim % 3] /= 2;
					tmp /= 2;

					--curDim;
				}
			}

			std::stringstream config;
			config << numNodes << "_";
			config << 6 << "_";
			config << 1 << "_";
			config << numPerDim[0] << "x";
			config << numPerDim[1] << "x";
			config << numPerDim[2] << "_";
			config << numOMP[0] << "x";
			config << numOMP[1] << "x";
			config << numOMP[2];

			{
				auto outFile = std::ofstream("JuQueen_" + config.str() + "_Settings.txt");
				outFile << "outputPath = \"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated_" << config.str() << "/\"" << std::endl
					<< "basePathPrefix = \".\"" << std::endl;

				outFile.close();
			}
			{
				auto outFile = std::ofstream("JuQueen_" + config.str() + "_Knowledge.txt");
				outFile << "targetCompiler = \"IBMXL\"" << std::endl
					<< "dimensionality = 3" << std::endl
					<< "maxLevel = 6" << std::endl
					<< "comm_strategyFragment = 6" << std::endl
					<< "domain_summarizeBlocks = true" << std::endl
					<< "domain_numBlocks_x = " << numPerDim[0] << std::endl
					<< "domain_numBlocks_y = " << numPerDim[1] << std::endl
					<< "domain_numBlocks_z = " << numPerDim[2] << std::endl
					<< "domain_numFragsPerBlock_x = " << numOMP[0] << std::endl
					<< "domain_numFragsPerBlock_y = " << numOMP[1] << std::endl
					<< "domain_numFragsPerBlock_z = " << numOMP[2] << std::endl
					<< "domain_fragLength_x = 1" << std::endl
					<< "domain_fragLength_y = 1" << std::endl
					<< "domain_fragLength_z = 1" << std::endl;

				outFile.close();
			}

			std::cout << config.str() << " ";
			++numConfigs;

			jobScript << "export CONFIG=" << config.str() << "\n"
				<< "export OMP_NUM_THREADS=" << numOMPTotal << "\n"
				<< "cd $HOME/Exa/Generated_Weak_Hybrid\n"
				<< "cp Poisson_$CONFIG $WORK/Exa_Generated_Weak/Poisson_$CONFIG\n"
				<< "cd $WORK/Exa_Generated_Weak\n"
				<< "time runjob --ranks-per-node " << 64 / numOMPTotal << " --np " << numNodes * 64 / numOMPTotal << " --exp-env OMP_NUM_THREADS : ./Poisson_$CONFIG\n";
		}

		jobScript.close();
	}

	std::cout << "\nGenerated " << numConfigs << " configs\n";

	return 0;
}
